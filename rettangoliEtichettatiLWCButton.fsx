#load "LWC.fsx"
open LWC
open System.Windows.Forms
open System.Drawing

let f = new Form(Text="prova", TopMost=true)
    
type RettEtichetta() =
    let mutable vuoto = false //l'elemento non è vuoto
    let mutable rett = new Rectangle(0,0,10,10)
    let mutable nome = ""
    let mutable font = new Font("Arial", 12.f)

    let mutable selezionato = false

    //let mutable fd:RettEtichetta = new RettEtichetta();

    member this.Vuoto
      with get() = vuoto
      and set(v) = vuoto <- v

    member this.Rett
      with get() = rett
      and set(v) = rett <- v

    member this.Nomina
      with get() = nome
      and set(v) = nome <- v
    
    member this.AggFont
      with get() = font
      and set(v) = font <- v

    member this.Seleziona
        with get() = selezionato
        and set(v) = selezionato <- v

type ArchEtichetta() =
    let mutable nome = ""
    let mutable nodo1 = new RettEtichetta()
    let mutable nodo2 = new RettEtichetta()
    
    member this.Points =
        let jLoc = nodo1.Rett.Location
        let jRett = nodo1.Rett
        let iLoc = nodo2.Rett.Location
        let iRett = nodo2.Rett
        let mutable diff = 0
        if iLoc.Y>jLoc.Y then
            diff <- iLoc.Y - jLoc.Y
        else
            diff <- jLoc.Y - iLoc.Y
        if diff>=0 && diff<=iRett.Height then
            if iLoc.X > jLoc.X then //j sta a sx
                PointF(float32(iLoc.X),float32(iLoc.Y+iRett.Height/2)),PointF(float32(jLoc.X+jRett.Width),float32(jLoc.Y+iRett.Height/2))
            else
                PointF(float32(iLoc.X+iRett.Width),float32(iLoc.Y+iRett.Height/2)),PointF(float32(jLoc.X),float32(jLoc.Y+iRett.Height/2))
         else
            if iLoc.Y < jLoc.Y then //j sta sotto
                PointF(float32(iLoc.X+iRett.Width/2),float32(iLoc.Y+iRett.Height)),PointF(float32(jLoc.X+jRett.Width/2),float32(jLoc.Y))
            else //sta sopra
                PointF(float32(iLoc.X+iRett.Width/2),float32(iLoc.Y)),PointF(float32(jLoc.X+jRett.Width/2),float32(jLoc.Y+jRett.Height))
    
    member this.Nomina
      with get() = nome
      and set(v) = nome <- v

    member this.Nodo1 
        with get() = nodo1
        and set(v) = nodo1 <- v
    
    member this.Nodo2 
        with get() = nodo2
        and set(v) = nodo2 <- v

let checkPickCorrelationArch (rett1:RettEtichetta) (rett2:RettEtichetta) (p:Point) = //pick correlation di un punto su un rettangolo
    let mutable esitoX = 0
    let mutable esitoY = 0
    if (rett2.Rett.X - rett1.Rett.X) <> 0 then
         esitoX <- (p.X - rett1.Rett.X)/(rett2.Rett.X - rett1.Rett.X)
    if (rett2.Rett.Y - rett1.Rett.Y) <> 0 then
        esitoY <- (p.Y - rett1.Rett.Y)/(rett2.Rett.Y - rett1.Rett.Y)
    printfn "%A %A" esitoX esitoY
    if esitoX = esitoY then 
        true
    else
        if esitoX = 0 then
            if (p.Y - esitoY) = 0  then
                true
            else false
        else
            if esitoY = 0 then
                if( p.X - esitoX) = 0 then
                    true
                else false
            else false

let rec trova1 (l:ArchEtichetta list) (e:Point) =  //trova se stiamo cliccando in uno degli archi della lista
    match l with
    | [] -> 0
    | head :: tail -> 
    if not ( checkPickCorrelationArch head.Nodo1 head.Nodo2 e ) then 
        trova1 tail e
    else 1   
    
let mutable ind = new RettEtichetta(Vuoto=true) //elemento della lista da dover droppare
let mutable listaRett = []
let mutable listaArch = []

let checkDoubleArch (l:ArchEtichetta list) (elemento: ArchEtichetta) =
    let mutable found:int = 0
    for i in l do
        if (i.Nodo1=elemento.Nodo1 && i.Nodo2=elemento.Nodo2) || (i.Nodo2=elemento.Nodo1 && i.Nodo1=elemento.Nodo2) then
            found <- 1
    found

type Rettangoli() =
    inherit LWCControl()

    let mutable stillclick = false
    override this.OnMouseDown(e) =
        printfn "%A" (trova1 listaArch e.Location)

        let px = e.Location.X
        let py = e.Location.Y
        if listaRett.IsEmpty then //viene aggiunto il rettangolo per la prima volta
            let etichetta = new RettEtichetta(Nomina="pippo")
            etichetta.Rett <- new Rectangle(px,py,int(etichetta.AggFont.Size)*etichetta.Nomina.Length,int(etichetta.AggFont.Height))
            listaRett <- List.append listaRett [ etichetta ] //l'= va solo la prima volta che viene aggiunto l'elemento
            ind <- etichetta
            ind.Seleziona <- true
            this.Invalidate()
        else
            let rec trova (l:RettEtichetta list) =  //trova se stiamo cliccando in uno dei rettangoli della lista
                match l with
                | [] -> new RettEtichetta(Vuoto=true)
                | head :: tail -> 
                if not ( this.checkPickCorrelationRett head.Rett e.Location ) then 
                    trova tail
                else head

            let selected = (trova listaRett )
            if selected.Vuoto then //aggiungi un rettangolo nuovo perché ho selezionato area vuota/nessun rettangolo già presente
                let etichetta = new RettEtichetta(Nomina="pippo")
                etichetta.Rett <- new Rectangle(px,py,int(etichetta.AggFont.Size)*etichetta.Nomina.Length,int(etichetta.AggFont.Height))
                listaRett <- List.append listaRett [ etichetta  ] 
                ind <- etichetta
                ind.Seleziona <- true
            else if ind.Vuoto then //seleziona un rettangolo per la prima volta da spostare
                ind <- selected //ind diventa il rettangolo che già esiste che vogliamo spostare
                ind.Seleziona <- true
            else if ind<>selected then //se è già selezionato aggiungerne arco
                let elementoArco = new ArchEtichetta(Nomina="pluto",Nodo1=ind, Nodo2=selected)
                if (checkDoubleArch listaArch elementoArco ) = 0 then
                    listaArch <- List.append listaArch [ elementoArco ]
                ind <- selected
                ind.Seleziona <- true
            else if ind = selected then   
                ind.Seleziona <- not(ind.Seleziona)
                if not(ind.Seleziona) then
                    ind <- new RettEtichetta(Vuoto=true)
            stillclick <- true           
            this.Invalidate()

    override this.OnMouseMove(e) =
        if not(ind.Vuoto) && stillclick then //muove rettangolo
            ind.Rett <- new Rectangle(e.Location.X, e.Location.Y, ind.Rett.Width, ind.Rett.Height) 
            this.Invalidate()

    override this.OnMouseUp(e) =
        //if stillclick then //se è stato mosso non interessa sapere che deve essere collegato
        //    ind <- new RettEtichetta(Vuoto=true)
        stillclick <- false

    override this.OnPaint(e) =
        let g = e.Graphics
        if ind.Seleziona then //seleziono rettangolo
            g.DrawRectangle(Pens.Black, new Rectangle(ind.Rett.X-1, ind.Rett.Y-1, ind.Rett.Width+1, ind.Rett.Height+1))
            //+,-1 perchè essendo fatta dopo la fillRectangle andava a coprire una parte del drawRectangle
        for i in listaRett do 
            g.FillRectangle(Brushes.Red, i.Rett )
            g.DrawString(i.Nomina, new Font("Arial", 12.f, FontStyle.Bold), Brushes.Green, PointF(float32(i.Rett.Location.X), float32(i.Rett.Location.Y)))
        for j in listaArch do
            let (point1, point2) = j.Points
            g.DrawLine(Pens.Black, point1, point2)

    override this.OnResize(e) =
        this.ClientSize <- SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height))
        this.Invalidate()
        base.OnResize e

type DeleteButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) =
        if ind.Seleziona then
            let mutable newLista = []
            let mutable newArco = []
            for i in listaRett do
                if ind <> i then
                    newLista <- List.append newLista [i]
            for i in listaArch do
                if not(i.Nodo1 = ind || i.Nodo2 = ind) then
                    newArco <- List.append newArco [i]
            listaArch <- newArco
            listaRett <- newLista
            
            ind <- new RettEtichetta(Vuoto=true) 
        this.Invalidate()

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Black, new Rectangle(0,0,50,50))

let textBox = new TextBox(Top=50, Left=0, Width=50)
type NameButton() =
    inherit LWCControl()
        
    override this.OnMouseUp(e) =    
        if ind.Seleziona && textBox.Text.Length>0 then
            ind.Nomina <- textBox.Text
            ind.Rett <- new Rectangle(ind.Rett.X,ind.Rett.Y,int(ind.AggFont.Size)*ind.Nomina.Length,int(ind.AggFont.Height))
            this.Invalidate() 

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Aquamarine, new Rectangle(0,0,50,50))
    
let lwcc = new LWCContainer(Dock=DockStyle.Fill)
let r = new Rettangoli(Position=PointF(50.f, 0.f),ClientSize=SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height)))
lwcc.LWControls.Add(r)
let d = new DeleteButton(Position=PointF(0.f, 0.f),ClientSize=SizeF(float32(50.f), float32(50.f)))
lwcc.LWControls.Add(d)
let n = new NameButton(Position=PointF(0.f, 50.f),ClientSize=SizeF(float32(50.f), float32(50.f)))
lwcc.LWControls.Add(n)
lwcc.Controls.Add(textBox)

f.Controls.Add(lwcc)
f.Show()