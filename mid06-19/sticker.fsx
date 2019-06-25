#load "LWC.fsx"
open LWC
open System.Windows.Forms
open System.Drawing

let f = new Form(Text="prova", TopMost=true)
f.Height <- 700
f.Width <- 700
f.BackColor <- Color.LightGray

let mutable LWCArea = new LWCControl() //per riferire l'area di disegno

type Buttons() =
    inherit LWCControl()

    let bottoni = [| new Rectangle(0,0,25,25); new Rectangle(25+5,0,25,25); new Rectangle(25*2+5*2,0,25,25); new Rectangle(25*3+5*3,0,25,25); new Rectangle(25*4+5*4,0,25,25); new Rectangle(25*5+5*5,0,25,25); new Rectangle(25*6+5*6,0,25,25); new Rectangle(25*7+5*7,0,25,25) |]
    let lettere = [| "←";"→";"↑";"↓";"+";"-";"⟲";"⟳" |]

    override this.OnMouseUp(e) =
        let mutable i = 0 
        while not(bottoni.[i].Contains(e.Location)) do
            i <- i + 1
        match i with
        | 0 -> printfn "hi"
        | 1 -> printfn "yes"
        | 2 -> printfn "no"
        | 3 -> printfn "fff"
        | 4 -> printfn "yesss"
        | 5 -> printfn "yes"
        | 6 -> printfn "yes"
        | 7 -> printfn "yes"

    override this.OnPaint(e) =
        let g = e.Graphics

        //g.FillRectangle(Brushes.DarkGray, new Rectangle(0,0,int this.Width,int this.Height))

        for i in 0 .. (bottoni.Length - 1) do
            g.FillRectangle(Brushes.DarkGray, bottoni.[i])
            g.DrawString(lettere.[i],new Font("Tahoma",float32 12.5),Brushes.Black,PointF(float32 bottoni.[i].X,0.f))

type Operazioni() =
    inherit LWCControl()

    let bottoni = [| new Rectangle(0,0,25,25); new Rectangle(25+5,0,25,25) |]
    let lettere = [| "📄";"→" |]

    override this.OnMouseUp(e) =
        let mutable i = 0 
        while not(bottoni.[i].Contains(e.Location)) do
            i <- i + 1
        match i with
        | 0 -> printfn "hi"
        | 1 -> printfn "yes"
        | 2 -> printfn "no"
        | 3 -> printfn "fff"
        | 4 -> printfn "yesss"
        | 5 -> printfn "yes"
        | 6 -> printfn "yes"
        | 7 -> printfn "yes"

    override this.OnPaint(e) =
        let g = e.Graphics

        g.FillRectangle(Brushes.DarkGray, new Rectangle(0,0,int this.Width,int this.Height))

        for i in 0 .. (bottoni.Length - 1) do
            g.FillRectangle(Brushes.DarkGray, bottoni.[i])
            g.DrawString(lettere.[i],new Font("Tahoma",float32 12.5),Brushes.Black,PointF(float32 bottoni.[i].X,0.f))

type Area() =
    inherit LWCControl()

    override this.OnPaint(e) =
        let g = e.Graphics

        g.FillRectangle(Brushes.White, new Rectangle(0,0,int this.Width,int this.Height))


let lwcc = new LWCContainer(Dock=DockStyle.Fill)

let AreaDraw = new Area(Position=PointF(100.f, 75.f),ClientSize=SizeF(float32(f.ClientSize.Width-150), float32(f.ClientSize.Height-100)))
let ButtonControl = new Buttons(Position=PointF(0.f, 0.f),ClientSize=SizeF(8.f*25.f + 5.f*7.f, 25.f))
let StickerControl = new Operazioni(Position=PointF(0.f, 30.f),ClientSize=SizeF(8.f*25.f + 5.f*7.f, 25.f))

LWCArea <- AreaDraw
lwcc.LWControls.Add(AreaDraw)
lwcc.LWControls.Add(ButtonControl)
lwcc.LWControls.Add(StickerControl)

f.Controls.Add(lwcc)
f.Show()