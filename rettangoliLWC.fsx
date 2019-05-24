#load "LWC.fsx"
open LWC

open System.Windows.Forms
open System.Drawing

let f = new Form(Text="prova", TopMost=true)

type Rettangoli() =
    inherit LWCControl()

    //la parte commentata aggiunge un solo rettangolo ogni volta che si clicca sulla form
    //let mutable rett = new Rectangle(0, 0, 30, 30)
    //let mutable disegna = false
    
    let mutable lista = [ ]
    let mutable ind = -1 //posizione elemento della lista da dover droppare

    override this.OnMouseDown(e) =
        let px = e.Location.X
        let py = e.Location.Y
        //rett <- new Rectangle(px, py, 30, 30)
        //disegna <- true 
        //this.Invalidate() //si fa ogni volta che vogliamo mostrare una modifica, comporta il richiamo della OnPaint

        if lista.IsEmpty then //viene aggiunto il rettangolo per la prima volta
            lista <- List.append lista [ new Rectangle(px, py, 30, 30) ] //l'= va solo la prima volta che viene aggiunto l'elemento
            this.Invalidate()
        else
            let rec trova l index = 
               match l with
               | [] -> -1
               | head :: tail -> 
               if not ( this.checkPickCorrelation head e.Location ) then 
                trova tail (index+1)
               else index

            ind <- (trova lista 0 )
            if ind = -1 then //aggiungo un nuovo rettangolo se non è presente in tale area
                lista <- List.append lista [ new Rectangle(px, py, 30, 30) ] 
                this.Invalidate()
                
    override this.OnMouseMove(e) =
        if ind > -1 then
            let mutable nuovaLista = []
            let mutable indCount = 0
            for i in lista do
                if indCount <> ind then
                    nuovaLista <- List.append nuovaLista [ i ]
                else    
                    nuovaLista <- List.append nuovaLista [ new Rectangle(e.Location.X, e.Location.Y, 30, 30) ]
                indCount <- indCount+1
            //lista <- []
            lista <- nuovaLista 

            this.Invalidate()

    override this.OnMouseUp(e) =
        ind <- -1

    override this.OnPaint(e) =
        let g = e.Graphics

        //if disegna then
            //g.FillRectangle(Brushes.Red, rett)

        for i in lista do 
            g.FillRectangle(Brushes.Red, i )

    override this.OnResize(e) =
        this.ClientSize <- SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height))
        this.Invalidate()
        base.OnResize e


let lwcc = new LWCContainer(Dock=DockStyle.Fill)
let r = new Rettangoli(ClientSize=SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height)))
lwcc.LWControls.Add(r)

f.Controls.Add(lwcc)

f.Show()