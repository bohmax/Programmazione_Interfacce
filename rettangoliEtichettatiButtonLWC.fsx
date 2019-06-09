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

    let mutable arco = [] //corrisponde ad una lista di RettEtichetta, per ogni rettangolo sappiamo a chi altro è connesso
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

    member this.Arco
        with get() = arco
        and set(v) = arco <- v

    member this.Seleziona
        with get() = selezionato
        and set(v) = selezionato <- v

type Rettangoli() =
    inherit LWCControl()

    let mutable lista = []
    let mutable ind = new RettEtichetta(Vuoto=true) //elemento della lista da dover droppare
    let mutable stillclick = false

    override this.OnMouseDown(e) =
        let px = e.Location.X
        let py = e.Location.Y
        if lista.IsEmpty then //viene aggiunto il rettangolo per la prima volta
            let etichetta = new RettEtichetta(Nomina="pippo")
            etichetta.Rett <- new Rectangle(px,py,int(etichetta.AggFont.Size)*etichetta.Nomina.Length,int(etichetta.AggFont.Height))
            lista <- List.append lista [ etichetta ] //l'= va solo la prima volta che viene aggiunto l'elemento
            this.Invalidate()
        else
            let rec trova (l:RettEtichetta list) =  //trova se stiamo cliccando in uno dei rettangoli della lista
                match l with
                | [] -> new RettEtichetta(Vuoto=true);
                | head :: tail -> 
                if not ( this.checkPickCorrelation head.Rett e.Location ) then 
                    trova tail
                else head

            let selected = (trova lista )
            if selected.Vuoto then //aggiungi un rettangolo nuovo perché ho selezionato area vuota/nessun rettangolo già presente
                let etichetta = new RettEtichetta(Nomina="pippo")
                etichetta.Rett <- new Rectangle(px,py,int(etichetta.AggFont.Size)*etichetta.Nomina.Length,int(etichetta.AggFont.Height))
                lista <- List.append lista [ etichetta  ] 
            else if ind.Vuoto then //seleziona un rettangolo per la prima volta da spostare
                ind <- selected //ind diventa il rettangolo che già esiste che vogliamo spostare
                ind.Seleziona <- true
            else if ind<>selected then //se è già selezionato aggiungerne arco
                let mutable cnt=0
                for i in ind.Arco do //aggiungiamo un arco per un nodo non ancora connesso
                    if i = selected then
                        cnt <- 1
                if cnt = 0 then
                    ind.Arco <- List.append ind.Arco [ selected ] //assicurarsi che l'elemento non è già stato inserito precedentemente
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
            ind.Rett <- new Rectangle(e.Location.X, e.Location.Y, ind.Rett.Width, ind.Rett.Height) //bug: si rischia di muovere impropriamente un quadrato, perchè?
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
        for i in lista do 
            g.FillRectangle(Brushes.Red, i.Rett )
            g.DrawString(i.Nomina, new Font("Arial", 12.f, FontStyle.Bold), Brushes.Green, PointF(float32(i.Rett.Location.X), float32(i.Rett.Location.Y)))
            let iLoc = i.Rett.Location
            let iRett = i.Rett
            for j in i.Arco do
                let jLoc = j.Rett.Location
                let jRett = j.Rett
                let mutable diff = 0
                if iLoc.Y>jLoc.Y then
                    diff <- iLoc.Y - jLoc.Y
                else
                    diff <- jLoc.Y - iLoc.Y
                if diff>=0 && diff<=iRett.Height then
                    if iLoc.X > jLoc.X then //j sta a sx
                        g.DrawLine(Pens.Black,iLoc.X,iLoc.Y+iRett.Height/2,jLoc.X+jRett.Width,jLoc.Y+iRett.Height/2) 
                    else
                        g.DrawLine(Pens.Black,iLoc.X+iRett.Width,iLoc.Y+iRett.Height/2,jLoc.X,jLoc.Y+iRett.Height/2) 
                else
                    if iLoc.Y < jLoc.Y then //j sta sotto
                        g.DrawLine(Pens.Black,iLoc.X+iRett.Width/2,iLoc.Y+iRett.Height,jLoc.X+jRett.Width/2,jLoc.Y) 
                    else //sta sopra
                        g.DrawLine(Pens.Black,iLoc.X+iRett.Width/2,iLoc.Y,jLoc.X+jRett.Width/2,jLoc.Y+jRett.Height) 


    override this.OnResize(e) =
           this.ClientSize <- SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height))
           this.Invalidate()
           base.OnResize e

let lwcc = new LWCContainer(Dock=DockStyle.Fill)
let r = new Rettangoli(Position=PointF(50.f, 0.f), ClientSize=SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height)))
lwcc.LWControls.Add(r)
f.Controls.Add(lwcc)
f.Show()