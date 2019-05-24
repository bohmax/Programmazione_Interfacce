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

    let mutable arco = [] //corrisponde ad una lista di RettEtichetta
    let mutable daCollegare = false

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

    member this.Collega
        with get() = daCollegare
        and set(v) = daCollegare <- v

type Rettangoli() =
    inherit LWCControl()

    let mutable lista = [ ]
    let mutable ind = new RettEtichetta() //elemento della lista da dover droppare
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
            if selected.Vuoto then //aggiungi un elemento
                let etichetta = new RettEtichetta(Nomina="pippo")
                etichetta.Rett <- new Rectangle(px,py,int(etichetta.AggFont.Size)*etichetta.Nomina.Length,int(etichetta.AggFont.Height))
                lista <- List.append lista [ etichetta  ] 
                this.Invalidate()
                ind <- new RettEtichetta(Vuoto=true)
            else if ind.Vuoto then //seleziona un elemento
                ind <- selected
            else if ind<>selected then //se è già selezionato aggiungerne un altro
                ind.Arco <- List.append ind.Arco [ selected ] //assicurarsi che l'elemento non è già stato inserito precedentemente
                this.Invalidate()
            
            stillclick <- true           

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
        let mutable ind1 = new RettEtichetta()
        let mutable ind2 = new RettEtichetta()
        for i in lista do 
            //printfn "%A" i.Rett
            g.FillRectangle(Brushes.Red, i.Rett )
            g.DrawString(i.Nomina, new Font("Arial", 12.f, FontStyle.Bold), Brushes.Green, PointF(float32(i.Rett.Location.X), float32(i.Rett.Location.Y)))
            let iPosition = i.Rett.Location
            for j in i.Arco do
                let jPosition = j.Rett.Location
                g.DrawLine(Pens.Black,iPosition.X,iPosition.Y,jPosition.X,jPosition.Y) //si può disegnare meglio, ti lascio questo compito a casa se saprai di questo commento so che almeno ci hai pensato
            //if i.Collega then
                //if not(ind1.Vuoto) then
                  //  ind1 <- i
                //else
                  //  if not(ind2.Vuoto) then
                      //  ind2 <- i
                    //else    
                        //g.DrawLine(Pens.Black, Point(ind1.Rett.Width, ind1.Rett.Height), Point(ind2.Rett.Width, ind2.Rett.Height))
                        //ind1 <- new RettEtichetta()
                        //ind2 <- new RettEtichetta()
                        //i.Collega <- false

let lwcc = new LWCContainer(Dock=DockStyle.Fill)
let r = new Rettangoli(ClientSize=SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height)))
lwcc.LWControls.Add(r)

f.Controls.Add(lwcc)

f.Show()