#load "LWC.fsx"
open LWC
open System.Windows.Forms
open System.Drawing

let f = new Form(Text="prova", TopMost=true)
f.Height <- 700
f.Width <- 700
f.BackColor <- Color.LightGray
    
type RettEtichetta() =
    let mutable nome = ""
    let mutable font = new Font("Arial", 12.f)
    let mutable vuoto = false //l'elemento non è vuoto
    let mutable rett = new Rectangle(0,0,10,10)
    let mutable seleziona = false
    let mutable insieme = 0

    member this.Nomina
     with get() = nome
     and set(v) = nome <- v

    member this.AggFont
     with get() = font
     and set(v) = font <- v

    member this.Vuoto
      with get() = vuoto
      and set(v) = vuoto <- v

    member this.Rett
      with get() = rett
      and set(v) = rett <- v
 
    member this.Seleziona
        with get() = seleziona
        and set(v) = seleziona <- v

    member this.Insieme
        with get() = insieme
        and set(v) = insieme <- v

type ArchEtichetta() =
    let mutable nome = ""
    let mutable font = new Font("Arial", 12.f, FontStyle.Bold)
    let mutable vuoto = false //l'elemento non è vuoto
    let mutable nodo1 = new RettEtichetta()
    let mutable nodo2 = new RettEtichetta()
    let mutable seleziona = false

    member this.Nomina
      with get() = nome
      and set(v) = nome <- v
    
    member this.AggFont
     with get() = font
     and set(v) = font <- v

    member this.Vuoto
      with get() = vuoto
      and set(v) = vuoto <- v
    
    member this.Nodo1 
     with get() = nodo1
     and set(v) = nodo1 <- v
    
    member this.Nodo2 
     with get() = nodo2
     and set(v) = nodo2 <- v

    member this.Points = //punti di disegno dell'arco
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

    member this.Seleziona
        with get() = seleziona
        and set(v) = seleziona <- v

let checkPickCorrelationRett (rett:Rectangle) (p:Point) = //pick correlation di un punto su un rettangolo
    if rett.Contains p then 
        true
    else false

let rec trovaRett (l:RettEtichetta list) (p:Point) =  //trova se stiamo cliccando in uno dei rettangoli della lista
    match l with
    | [] -> new RettEtichetta(Vuoto=true)
    | head :: tail -> 
    //if not ( this.checkPickCorrelationRett head.Rett p ) then //se usa quella dei LWC
    if not ( checkPickCorrelationRett head.Rett p ) then 
        trovaRett tail p
    else head

let checkPickCorrelationArch (arco:ArchEtichetta) (p:Point) = //pick correlation di un punto su un arco
    let (punto1, punto2) = arco.Points
    let minX = int( min punto1.X punto2.X)
    let maxX = int( max punto1.X punto2.X)
    let minY = int( min punto1.Y punto2.Y)
    let maxY = int( max punto1.Y punto2.Y)
    if p.X>=minX && p.X<=maxX && p.Y>=minY && p.Y<= maxY then
        let mutable esitoX = 0.f
        let mutable esitoY = 0.f
        let mutable diff = 0.f
        //printfn "%A %A %A" punto1 punto2 p
        if (punto2.X - punto1.X) = 0.f then
            diff <- (float32(p.X) - punto1.X)
        else if (punto2.Y - punto1.Y) = 0.f then
            diff <- (float32(p.Y) - punto1.Y)
        else
            esitoX <- float32((float32(p.X) - punto1.X)/(punto2.X - punto1.X))
            esitoY <- float32((float32(p.Y) - punto1.Y)/(punto2.Y - punto1.Y))
            diff <- esitoX - esitoY
        if abs(diff) < 0.05f then 
            true
        else false
    else false

let rec trovaArch (l:ArchEtichetta list) (e:Point) =  //trova se stiamo cliccando in uno degli archi della lista
    match l with
    | [] -> new ArchEtichetta(Vuoto=true)
    | head :: tail -> 
    if not ( checkPickCorrelationArch head e ) then 
        trovaArch tail e
    else head   

let checkDoubleArch (l:ArchEtichetta list) (elemento: ArchEtichetta) = //controllo se c'è un arco duplicato
    let mutable found:int = 0
    for i in l do
        if (i.Nodo1=elemento.Nodo1 && i.Nodo2=elemento.Nodo2) || (i.Nodo2=elemento.Nodo1 && i.Nodo1=elemento.Nodo2) then
            found <- 1
    found

let classificaInsieme (l:RettEtichetta list) (insiemeMinimo: int) (insiemeDaMinimizzare: int) = 
    for i in l do
        if ( i.Insieme = insiemeDaMinimizzare ) then    
            i.Insieme <- insiemeMinimo

let lastElement (l:int list) = 
    let mutable ultimo = 0
    for i in l do
        ultimo <- i
    ultimo

let mutable listaRett = [] //lista oggetti RettEtichetta nell'Area
let mutable listaArch = [] //lista oggetti ArchEtichetta nell'Area
let mutable indRettSelected = new RettEtichetta(Vuoto=true) //elemento della listaRett da dover spostare/selezionare
let mutable indArchSelected = new ArchEtichetta(Vuoto=true) //elemento della listaArch da dover spostare/selezionare
let mutable sposta = new PointF(0.f,0.f) //per sapere di quando fare la translate dopo la onpaint
let mutable LWCArea = new LWCControl()

let mutable contaRotate = 0 //perché il clipping funziona soltanto su coordinate normalizzate per cui per spostare a dx, sx su un oggetto obliquo 
    //la continua a spostare in obliquo e non a dx o sx

let mutable listaInsiemiConnessi = [] //lista nodi tra loro connessi

let trovaInsieme (lI:int list) (ins: int) =
    let mutable result = false
    for i in lI do
        if i=ins then 
            result <- true
        else result <- result
    result

let aggiornaListaInsiemiConnessi (lR:RettEtichetta list) (lA:ArchEtichetta list) = //sono state aggiornate dopo l'eliminazione
    let mutable contatore = 0
    let mutable listaInsiemi = []
    for i in lR do //i nodi hanno un contatore insieme tra loro diversi
        contatore <- contatore + 1
        i.Insieme <- contatore
    for i in lA do
        if i.Nodo1.Insieme <> i.Nodo2.Insieme then
            let min = min i.Nodo1.Insieme i.Nodo2.Insieme
            let max = max i.Nodo1.Insieme i.Nodo2.Insieme
            classificaInsieme listaRett min max
    for i in lR do //aggiorno listaInsiemiConnessi con i contatori nuovi
        if not(trovaInsieme listaInsiemi i.Insieme) then
            listaInsiemi <- List.append listaInsiemi [i.Insieme]
    listaInsiemiConnessi <- listaInsiemi

type Area() =
    inherit LWCControl()

    let mutable stillclick = false
    
    override this.OnMouseDown(e) =
        if listaRett.IsEmpty then //viene aggiunto RettEtichetta per la prima volta
            let rettNuovo = new RettEtichetta(Nomina="vuoto", Seleziona=true, Insieme=1)
            rettNuovo.Rett <- new Rectangle(e.Location.X,e.Location.Y,int(rettNuovo.AggFont.Size)*rettNuovo.Nomina.Length,int(rettNuovo.AggFont.Height))
            listaRett <- List.append listaRett [ rettNuovo ] //l'= va solo la prima volta che viene aggiunto l'elemento
            listaInsiemiConnessi <- List.append listaInsiemiConnessi [1]
            indRettSelected <- rettNuovo 
            this.Invalidate()
        else
            let selectedRett = (trovaRett listaRett e.Location) //controllo se abbiamo selezionato un RettEtichetta 
            if selectedRett.Vuoto then 
                let selectedArch = (trovaArch listaArch e.Location) //controllo se abbiamo selezionato un ArchEtichetta
                if selectedArch.Vuoto then                
                    indArchSelected.Seleziona <- false
                    indArchSelected <- new ArchEtichetta(Vuoto=true) //garanzia ulteriore che lo abbiamo deselezionato
                    let rettNuovo = new RettEtichetta(Nomina="vuoto", Seleziona=true, Insieme= lastElement(listaInsiemiConnessi) + 1) //aggiungi un RettEtichetta perché ho selezionato area vuota/nessun RettEtichetta già presente
                    rettNuovo.Rett <- new Rectangle(e.Location.X,e.Location.Y,int(rettNuovo.AggFont.Size)*rettNuovo.Nomina.Length,int(rettNuovo.AggFont.Height))
                    listaRett <- List.append listaRett [ rettNuovo  ] 
                    listaInsiemiConnessi <- List.append listaInsiemiConnessi [lastElement(listaInsiemiConnessi) + 1]
                    indRettSelected <- rettNuovo
                else 
                    indRettSelected.Seleziona <- false
                    indRettSelected <- new RettEtichetta(Vuoto=true) //garanzia che selezioniamo al massimo un oggetto alla volta
                    indArchSelected <- selectedArch //seleziona un ArchEtichetta per la prima volta 
                    indArchSelected.Seleziona <- true //diventa quello che già esiste                 
            else if indRettSelected.Vuoto then //seleziona un RettEtichetta per la prima volta 
                indArchSelected.Seleziona <- false //deseleziono quello precedentemente selezionato
                indArchSelected <- new ArchEtichetta(Vuoto=true) //garanzia che selezioniamo al massimo un oggetto alla volta                 
                indRettSelected.Seleziona <- false
                indRettSelected <- selectedRett //diventa quello che già esiste
                indRettSelected.Seleziona <- true
            else if indRettSelected<>selectedRett then //se è già selezionato un RettEtichetta aggiungerne arco
                let elementoArco = new ArchEtichetta(Nomina="empty",Nodo1=indRettSelected, Nodo2=selectedRett)
                if (checkDoubleArch listaArch elementoArco ) = 0 then
                    if elementoArco.Nodo1.Insieme <> elementoArco.Nodo2.Insieme then
                        let min = min elementoArco.Nodo1.Insieme elementoArco.Nodo2.Insieme
                        let max = max elementoArco.Nodo1.Insieme elementoArco.Nodo2.Insieme
                        classificaInsieme listaRett min max 
                    listaArch <- List.append listaArch [ elementoArco ]
                indArchSelected.Seleziona <- false 
                indArchSelected <- new ArchEtichetta(Vuoto=true)
                indRettSelected.Seleziona <- false
                indRettSelected <- selectedRett
                indRettSelected.Seleziona <- true
            else if indRettSelected = selectedRett then   
                indRettSelected.Seleziona <- not(indRettSelected.Seleziona)
                if not(indRettSelected.Seleziona) then
                    indRettSelected <- new RettEtichetta(Vuoto=true)
            stillclick <- true           
            this.Invalidate()

    override this.OnMouseMove(e) =
        if not(indRettSelected.Vuoto) && stillclick then //muove RettEtichetta
            let distanzaX = e.Location.X - indRettSelected.Rett.X
            let distanzaY = e.Location.Y - indRettSelected.Rett.Y
            for i in listaRett do
                if i.Insieme = indRettSelected.Insieme then
                    //if i <> indRettSelected then
                        i.Rett <- new Rectangle(i.Rett.X+distanzaX, i.Rett.Y+distanzaY, indRettSelected.Rett.Width, indRettSelected.Rett.Height)
                    //else //se lo faccio senza questo if si mette a flickerare se vuoi provalo, senno fidati
                       //indRettSelected.Rett <- new Rectangle(e.Location.X, e.Location.Y, indRettSelected.Rett.Width, indRettSelected.Rett.Height) 
            this.Invalidate()

    override this.OnMouseUp(e) =
        //if stillclick then //se è stato mosso non interessa sapere che deve essere collegato
        //    indRettSelected <- new RettEtichetta(Vuoto=true)
        stillclick <- false

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.White, new Rectangle(0,0,int this.Width,int this.Height))

        for i in listaRett do 
            g.FillRectangle(Brushes.Red, i.Rett )
            g.DrawString(i.Nomina, new Font("Arial", 12.f, FontStyle.Bold), Brushes.Green, PointF(float32(i.Rett.Location.X), float32(i.Rett.Location.Y)))
        if indRettSelected.Seleziona then //disegno selezione RettEtichetta
            g.DrawRectangle(Pens.Black, new Rectangle(indRettSelected.Rett.X, indRettSelected.Rett.Y, indRettSelected.Rett.Width, indRettSelected.Rett.Height))
        for j in listaArch do
            let (point1, point2) = j.Points
            g.DrawLine(Pens.Black, point1, point2)
            let mezzoX = (point1.X+point2.X)/2.f
            let mezzoY = (point1.Y+point2.Y)/2.f
            g.DrawString(j.Nomina, j.AggFont, Brushes.Green, PointF(float32(mezzoX), float32(mezzoY)))
        if indArchSelected.Seleziona then //disegno selezione ArchEtichetta
            let (point1, point2) = indArchSelected.Points
            g.DrawLine(Pens.Blue, point1, point2)    
        //this.WV.TranslateW(-sposta.X,-sposta.Y)
        //g.Transform <- this.WV.WV
        //sposta <- PointF(0.f,0.f)

    override this.OnResize(e) =
        this.ClientSize <- SizeF(float32(f.ClientSize.Width-150), float32(f.ClientSize.Height-100))
        this.Invalidate()
        base.OnResize e

type DeleteButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) =
        if indRettSelected.Seleziona then
            let mutable newLista = []
            let mutable newArco = []
            for i in listaRett do
                if indRettSelected <> i then
                    newLista <- List.append newLista [i]
            for i in listaArch do
                if not(i.Nodo1 = indRettSelected || i.Nodo2 = indRettSelected) then
                    newArco <- List.append newArco [i]
            listaArch <- newArco
            listaRett <- newLista            
            indRettSelected <- new RettEtichetta(Vuoto=true) 
        if indArchSelected.Seleziona then
            let mutable newArco = []
            for i in listaArch do
                if not(i = indArchSelected) then
                    newArco <- List.append newArco [i]
            listaArch <- newArco
            indArchSelected <- new ArchEtichetta(Vuoto=true)
        aggiornaListaInsiemiConnessi listaRett listaArch
        this.Invalidate()

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.AliceBlue, new Rectangle(0,0,50,50))
        g.DrawString("Delete", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

let textBox = new TextBox(Top=50, Left=0, Width=50)
type NameButton() =
    inherit LWCControl() 
    
    override this.OnMouseUp(e) =    
        if indRettSelected.Seleziona && textBox.Text.Length>0 then
            indRettSelected.Nomina <- textBox.Text                              //il seguente viene usato per i nomi lunghi
            indRettSelected.Rett <- new Rectangle(indRettSelected.Rett.X,indRettSelected.Rett.Y,int(indRettSelected.AggFont.Size)*indRettSelected.Nomina.Length,int(indRettSelected.AggFont.Height))
            //printfn "%A" indRettSelected.Insieme //non sono presenti in ordine lessicografico, il numero non ha significato seantico, funge da nome
        if indArchSelected.Seleziona && textBox.Text.Length>0 then
            indArchSelected.Nomina <- textBox.Text
        this.Invalidate() 

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Aquamarine, new Rectangle(0,0,50,50))
        g.DrawString("Name", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ZommaAumentaButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) =
        LWCArea.WV.ScaleW(1.f*float32 1.1,1.f*float32 1.1)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Yellow, new Rectangle(0,0,50,50))
        g.DrawString("Zomma+", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ZommaDecrementaButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.ScaleW(1.f/float32 1.1,1.f/float32 1.1)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Orange, new Rectangle(0,0,50,50))
        g.DrawString("Zomma-", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type RotateDestraButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.RotateW(float32 -contaRotate)
        contaRotate <- (contaRotate-45)%360
        LWCArea.WV.RotateW(float32 contaRotate)
        //sposta <- PointF(-LWCArea.Width/2.f, -LWCArea.Height/2.f)
        this.Invalidate() //notifica al SistemaGrafico che stai modificando qualcosa, 
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Blue, new Rectangle(0,0,50,50))
        g.DrawString("RotateDX", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))


type RotateSinistraButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) =
        LWCArea.WV.RotateW(float32 -contaRotate)
        contaRotate <- (contaRotate+45)%360
        LWCArea.WV.RotateW(float32 contaRotate)
        this.Invalidate()        
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Green, new Rectangle(0,0,50,50))
        g.DrawString("RotateSX", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

//vengono implementati anche con la scrollbar
type ScorriDestraButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(10.f, 0.f)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Bisque, new Rectangle(0,0,50,50))
        g.DrawString("ScorriDX", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ScorriSinistraButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(-10.f, 0.f)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Chocolate, new Rectangle(0,0,50,50))
        g.DrawString("ScorriSX", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ScorriAltoButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(0.f, -10.f)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.AliceBlue, new Rectangle(0,0,50,50))
        g.DrawString("ScorriSu", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ScorriBassoButton() = 
    inherit LWCControl()

    override this.OnMouseUp(e) = 
        LWCArea.WV.TranslateW(0.f, 10.f)
        this.Invalidate()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.AntiqueWhite, new Rectangle(0,0,50,50))
        g.DrawString("ScorriGiù", new Font("Arial", 8.f, FontStyle.Bold), Brushes.Green, PointF(0.f, 25.f))

type ScrollBarX() =
    inherit LWCControl()
    let mutable rettOrizzontale = new Rectangle(0,0,20,20)
    let mutable seleziona = false
    let mutable lastX = 0.f

    override this.OnMouseDown(e) =
        if checkPickCorrelationRett rettOrizzontale e.Location then
            seleziona <- true

    override this.OnMouseMove(e) =
        if seleziona && e.Button.Equals(MouseButtons.Left) && int(this.Width - 20.f) > e.Location.X then
            rettOrizzontale <- new Rectangle(e.Location.X, 0, 20, 20)
            let diff = float32 (float32 e.Location.X - lastX)
            LWCArea.WV.TranslateV(diff,0.f)
            lastX <- float32 e.Location.X
            this.Invalidate()

    override this.OnMouseUp(e) =
        seleziona <- false

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Gray, 0.f, 0.f, this.Width, this.Height)
        g.FillRectangle(Brushes.Black, rettOrizzontale)

    override this.OnResize(e) =
        this.ClientSize <- SizeF(float32 (f.ClientSize.Width-50-20), 20.f)
        this.Position <- PointF(50.f, float32 (f.ClientSize.Height - 20))
        this.Invalidate()
        base.OnResize e

type ScrollBarY() =
    inherit LWCControl()
    let mutable rettVerticale = new Rectangle(0,0,20,20)
    let mutable seleziona = false
    let mutable lastY = 0.f

    override this.OnMouseDown(e) =
        if checkPickCorrelationRett rettVerticale e.Location then
            seleziona <- true

    override this.OnMouseMove(e) =
        if seleziona && e.Button.Equals(MouseButtons.Left) && int(this.Height - 20.f) > e.Location.Y then
            //printfn "%A" e.Location.Y
            rettVerticale <- new Rectangle(0, e.Location.Y, 20, 20)
            let diff = float32 (float32 e.Location.Y - lastY)
            //let cw = LWCArea.WV.TransformPointW(PointF(0.f,float32 -e.Location.Y))
            //printfn "%A" cw
            LWCArea.WV.TranslateV(0.f, diff)
            lastY <- float32 e.Location.Y
            this.Invalidate()

    override this.OnMouseUp(e) =
        seleziona <- false

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Gray, 0.f, 0.f, this.Width, this.Height)
        g.FillRectangle(Brushes.Black, rettVerticale)

    override this.OnResize(e) =
        this.ClientSize <- SizeF(20.f, float32 (f.ClientSize.Height-20))
        this.Position <- PointF(float32 (f.ClientSize.Width-20), 0.f)
        this.Invalidate()
        base.OnResize e

type Disegno() =
    inherit LWCControl()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.DimGray, 0.f, 0.f, this.Width, this.Height)
        
    override this.OnResize(e) =
        this.Position <- PointF(float32 f.ClientSize.Width - 20.f, float32 f.ClientSize.Height - 20.f)
        this.Invalidate()
        base.OnResize e

 // -------------------------------- creazione container -------------------------------------------
let lwcc = new LWCContainer(Dock=DockStyle.Fill)
let r = new Area(Position=PointF(100.f, 50.f),ClientSize=SizeF(float32(f.ClientSize.Width-150), float32(f.ClientSize.Height-100)))
LWCArea <- r
lwcc.LWControls.Add(r)

let xS = new ScrollBarX(Position=PointF(50.f, float32 (f.ClientSize.Height - 20)),ClientSize=SizeF(float32 (f.ClientSize.Width-50-20), 20.f))
lwcc.LWControls.Add(xS)
let yS = new ScrollBarY(Position=PointF(float32 (f.ClientSize.Width-20), 0.f),ClientSize=SizeF(20.f, float32 (f.ClientSize.Height-20)))
lwcc.LWControls.Add(yS)
let cO = new Disegno(Position=PointF(float32 f.ClientSize.Width, float32 f.ClientSize.Height - 20.f),ClientSize=SizeF(20.f, 20.f)) //sbarra tra le due scrollbar
lwcc.LWControls.Add(cO)

let d = new DeleteButton(Position=PointF(0.f, 0.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(d)
let n = new NameButton(Position=PointF(0.f, 50.f),ClientSize=SizeF(50.f , 50.f))
lwcc.LWControls.Add(n)
lwcc.Controls.Add(textBox)
let zA = new ZommaAumentaButton(Position=PointF(0.f,100.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(zA)
let zD = new ZommaDecrementaButton(Position=PointF(0.f,150.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(zD)
let rD = new RotateDestraButton(Position=PointF(0.f,200.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(rD)
let rS = new RotateSinistraButton(Position=PointF(0.f,250.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(rS)
let sD = new ScorriDestraButton(Position=PointF(0.f,300.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(sD)
let sS = new ScorriSinistraButton(Position=PointF(0.f,350.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(sS)
let sA = new ScorriAltoButton(Position=PointF(0.f,400.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(sA)
let sB = new ScorriBassoButton(Position=PointF(0.f,450.f),ClientSize=SizeF(50.f, 50.f))
lwcc.LWControls.Add(sB)

//----------------------------------------------------------------------------------------------
f.Controls.Add(lwcc)
f.Show()
