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

    member this.AggArco
        with get() = arco
        and set(v) = arco <- v

    member this.Collega
        with get() = daCollegare
        and set(v) = daCollegare <- v

type Rettangoli() =
    inherit LWCControl()

    let mutable lista = [ ]
    let mutable ind = new RettEtichetta() //elemento della lista da dover droppare
    let mutable indMove = false 

    override this.OnMouseDown(e) =
        let px = e.Location.X
        let py = e.Location.Y
        if lista.IsEmpty then //viene aggiunto il rettangolo per la prima volta
            let etichetta = new RettEtichetta(Nomina="pippo")
            etichetta.Rett <- new Rectangle(px,py,int(etichetta.AggFont.Size)*etichetta.Nomina.Length,int(etichetta.AggFont.Height))
            lista <- List.append lista [ etichetta ] //l'= va solo la prima volta che viene aggiunto l'elemento
            this.Invalidate()
        else
            let rec trova (l:RettEtichetta list) index =  //trova se stiamo cliccando in uno dei rettangoli della lista
                match l with
                | [] -> new RettEtichetta(Vuoto=true);
                | head :: tail -> 
                if not ( this.checkPickCorrelation head.Rett e.Location ) then 
                    trova tail (index+1)
                else head

            if not(ind.Vuoto) then
                let selected = (trova lista 0 ) 
                if not(selected.Vuoto) && ind<>selected then  
                    ind.AggArco <- List.append (ind.AggArco) [ selected ]
            if ind.Vuoto then //aggiungo un nuovo rettangolo se non è presente in tale area
               let etichetta = new RettEtichetta(Nomina="pippo")
               etichetta.Rett <- new Rectangle(px,py,int(etichetta.AggFont.Size)*etichetta.Nomina.Length,int(etichetta.AggFont.Height))
               lista <- List.append lista [ etichetta  ] 
               this.Invalidate()
            indMove <- false           

    override this.OnMouseMove(e) =
        if not(ind.Vuoto) then //muove rettangolo
            indMove <- true //quindi non dobbiamo collegarlo
            ind.Rett <- new Rectangle(e.Location.X, e.Location.Y, ind.Rett.Width, ind.Rett.Height)
            this.Invalidate()        

    override this.OnMouseUp(e) =
        if indMove then //se è stato mosso non interessa sapere che deve essere collegato
            ind <- new RettEtichetta(Vuoto=true)            
        indMove <- false

    override this.OnPaint(e) =
        let g = e.Graphics
        let mutable ind1 = new RettEtichetta()
        let mutable ind2 = new RettEtichetta()
        for i in lista do 
            //printfn "%A" i.Rett
            g.FillRectangle(Brushes.Red, i.Rett )
            g.DrawString(i.Nomina, new Font("Arial", 12.f, FontStyle.Bold), Brushes.Green, PointF(float32(i.Rett.Location.X), float32(i.Rett.Location.Y)))
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