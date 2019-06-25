#load "LWC.fsx"
open LWC
open System.Windows.Forms
open System.Drawing

type Sticker() =
    inherit LWCControl()

    let mutable rettangolo = new Rectangle(-1,-1,0,0)
    let mutable testo = ""
    let mutable font = new Font("Arial", 12.f, FontStyle.Bold)
    let mutable image = []
    
    member this.Rettangolo 
        with get() = rettangolo
        and set(v) = rettangolo <- v
    member this.Testo 
        with get() = testo
        and set(v) = testo <- v
    member this.Font 
        with get() = font
        and set(v) = font <- v
    member this.Image 
        with get() = image
        and set(v) = image <- v
    member this.OnPaint(g:Graphics) = 
        let mutable count = 0
        for i in image do
            g.DrawImage(i, new Rectangle(rettangolo.X+5,rettangolo.Y+30*count+5,rettangolo.Width-10,30))
            count <- count + 1
        let mutable dimTesto = testo.Length
        let mutable posStringa = 0
        let mutable contaRiga = 0
        //let mutable textSize = TextRenderer.MeasureText(testo, font)
        //printfn "%A" textSize
        while dimTesto*int font.Size > 0 && contaRiga < 117 do
            let mutable lenght = 9
            if dimTesto < 10 then lenght <- dimTesto
            let nuovo = testo.Substring( posStringa , lenght)
            g.DrawString(nuovo,font, Brushes.Black, PointF(float32(rettangolo.X), float32(rettangolo.Y + contaRiga)))
            posStringa <- posStringa + 9
            dimTesto <- dimTesto - 9
            contaRiga <- contaRiga + 13
            //textSize <- TextRenderer.MeasureText(nuovo, font)
        

//------------------------------------funzioni di utilita----------------------------------

let checkPickCorrelationRett (rett:Rectangle) (p:Point) = //pick correlation di un punto su un rettangolo
    if rett.Contains p then 
        true
    else false

let rec trovaSticker (l:Sticker list) (e:Point) =  //trova se stiamo cliccando in uno degli archi della lista
    match l with
    | [] -> new Sticker()
    | head :: tail -> 
    if not ( checkPickCorrelationRett head.Rettangolo e ) then 
        trovaSticker tail e
    else head
//-----------------------------------------------------------------------------------------

let mutable LWCArea = new LWCControl() //per riferire l'area di disegno
let mutable listaSticker = [] //indica una lista degli sticker
let mutable selectedSticker = new Sticker()
let mutable lastMousePosition = Point(-1,-1)

type Buttons() =
    inherit LWCControl()

    let bottoni = [| new Rectangle(0,0,25,25); new Rectangle(25+5,0,25,25); new Rectangle(25*2+5*2,0,25,25); new Rectangle(25*3+5*3,0,25,25); new Rectangle(25*4+5*4,0,25,25); new Rectangle(25*5+5*5,0,25,25); new Rectangle(25*6+5*6,0,25,25); new Rectangle(25*7+5*7,0,25,25) |]
    let lettere = [| "←";"→";"↑";"↓";"+";"-";"⟲";"⟳" |]

    override this.OnMouseUp(e) =
        let mutable i = 0 
        let mutable trovato = false
        while i < bottoni.Length && not trovato do
            if not(bottoni.[i].Contains(e.Location)) then
                i <- i + 1
            else
                trovato <- true
                match i with
                | 0 -> printfn "←"
                | 1 -> printfn "→"
                | 2 -> printfn "↑"
                | 3 -> printfn "↓"
                | 4 -> printfn "+"
                | 5 -> printfn "-"
                | 6 -> printfn "⟲"
                | 7 -> printfn "⟳"

    override this.OnPaint(e) =
        let g = e.Graphics
        for i in 0 .. (bottoni.Length - 1) do
            g.FillRectangle(Brushes.DarkGray, bottoni.[i])
            g.DrawString(lettere.[i],new Font("Tahoma",float32 12.5),Brushes.Black,PointF(float32 bottoni.[i].X,0.f))

let textBox = new TextBox(Top=30, Left=60, Width=8*25 + 5*7 - 60)

type Operazioni() =
    inherit LWCControl()

    let bottoni = [| new Rectangle(0,0,25,25); new Rectangle(25+5,0,25,25) |]
    let lettere = [| "📄";"T" |]

    override this.OnMouseUp(e) =
        let mutable i = 0 
        let mutable trovato = false
        while i < bottoni.Length && not trovato do
            if not(bottoni.[i].Contains(e.Location)) then
                i <- i + 1
            else
                trovato <- true
                match i with
                | 0 -> 
                    printfn "📄"
                    if selectedSticker.Rettangolo.Height <> 0 && selectedSticker.Image.Length < 4 then
                        let mutable ofd = new OpenFileDialog()
                        if ofd.ShowDialog() = DialogResult.OK then
                            selectedSticker.Image <- List.append selectedSticker.Image [Image.FromFile ofd.FileName]
                            this.Invalidate()
                | 1 -> 
                    printfn "T"
                    if selectedSticker.Rettangolo.Height <> 0 && textBox.TextLength>0 then
                        selectedSticker.Testo <- textBox.Text
                        this.Invalidate()
                | 2 -> printfn "vuoto"
    

    override this.OnPaint(e) =
        let g = e.Graphics
        for i in 0 .. (bottoni.Length - 1) do
            g.FillRectangle(Brushes.DarkGray, bottoni.[i])
            g.DrawString(lettere.[i],new Font("Tahoma",float32 12.5),Brushes.Black,PointF(float32 bottoni.[i].X,0.f))

type Area() =
    inherit LWCControl()

    override this.OnMouseDown(e) =
        let risultatoricerca = trovaSticker listaSticker e.Location
        if risultatoricerca.Rettangolo.Height = 0 then //aggiungi nuovo sticker
            let st = new Sticker(Rettangolo = new Rectangle(e.Location.X,e.Location.Y,100,130))
            listaSticker <- List.append listaSticker [ st ]
            selectedSticker <- st
        else //seleziona lo steaker cliccato
            selectedSticker <- risultatoricerca
        lastMousePosition <- Point( e.X - selectedSticker.Rettangolo.X, e.Y - selectedSticker.Rettangolo.Y)
        this.Invalidate()                
    
    override this.OnMouseMove(e)  =  
        if selectedSticker.Rettangolo.Height <> 0 && e.Button.Equals(MouseButtons.Left) then
            selectedSticker.Rettangolo <- new Rectangle(e.X - lastMousePosition.X,e.Y - lastMousePosition.Y, 100, 130)
            this.Invalidate()

    //override this.OnMouseUp(e) =

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.White, new Rectangle(0,0,int this.Width,int this.Height))
        for i in listaSticker do
            if selectedSticker <> i then
                g.FillRectangle(Brushes.YellowGreen, i.Rettangolo)
                i.OnPaint(g)
        if selectedSticker.Rettangolo.Height <> 0 then
            g.FillRectangle(Brushes.YellowGreen, selectedSticker.Rettangolo)
            g.DrawRectangle(Pens.Black, selectedSticker.Rettangolo)
            selectedSticker.OnPaint g           

let f = new Form(Text="prova", TopMost=true)
f.Height <- 700
f.Width <- 700
f.BackColor <- Color.LightGray

let lwcc = new LWCContainer(Dock=DockStyle.Fill)

let AreaDraw = new Area(Position=PointF(100.f, 75.f),ClientSize=SizeF(float32(f.ClientSize.Width-150), float32(f.ClientSize.Height-100)))
let ButtonControl = new Buttons(Position=PointF(0.f, 0.f),ClientSize=SizeF(8.f*25.f + 5.f*7.f, 25.f))
let StickerControl = new Operazioni(Position=PointF(0.f, 30.f),ClientSize=SizeF(8.f*25.f + 5.f*7.f, 25.f))

LWCArea <- AreaDraw

lwcc.LWControls.Add(AreaDraw)
lwcc.LWControls.Add(ButtonControl)
lwcc.LWControls.Add(StickerControl)
lwcc.Controls.Add(textBox)

f.Controls.Add(lwcc)
f.Show()