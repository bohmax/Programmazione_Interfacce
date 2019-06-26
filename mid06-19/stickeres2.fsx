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
        let mutable contaAltezza = rettangolo.Y + 30*count + 5
        let mutable todraw = testo

        let mutable size = TextRenderer.MeasureText(testo, font); //studio la dimensione del testo in anticipo

        while size.Width <> 0 && (contaAltezza + size.Height - 5) < (rettangolo.Height + rettangolo.Y) && posStringa < testo.Length do

            let mutable numlettere = 0 //numero lettere usate per disegnare
            
            while numlettere < (dimTesto - posStringa) && TextRenderer.MeasureText(testo.Substring( posStringa , numlettere), font).Width < rettangolo.Width - 5 do
                numlettere <- numlettere + 1

            todraw <- testo.Substring( posStringa , numlettere)
            g.DrawString(todraw,font, Brushes.Black, PointF(float32(rettangolo.X-2), float32(contaAltezza)))
            size <- TextRenderer.MeasureText(todraw,font)
            contaAltezza <- contaAltezza + size.Height
            posStringa <- posStringa + numlettere
 
        

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

let mutable listaSticker = [] //indica una lista degli sticker
let mutable selectedSticker = new Sticker()
let mutable lastMousePosition = Point(-1,-1)

type Area() =
    inherit LWCContainer()

    let wv = WVMatrix()

    member this.WV
        with get() = wv
    
    override this.OnMouseDown(e) =
        let p = wv.TransformPointV(PointF(single e.X, single e.Y))
        let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
        let risultatoricerca = trovaSticker listaSticker evt.Location
        if risultatoricerca.Rettangolo.Height = 0 then //aggiungi nuovo sticker
            let st = new Sticker(Rettangolo = new Rectangle(evt.Location.X,evt.Location.Y,100,130))
            listaSticker <- List.append [ st ] listaSticker
            selectedSticker <- st
        else //seleziona lo steaker cliccato
            selectedSticker <- risultatoricerca
            listaSticker <- List.append [selectedSticker] listaSticker //in modo da avere l'elemento selezionato in cima alla lista
            listaSticker <- List.distinct listaSticker //elimino il duplicato dopo l'inserimento
        lastMousePosition <- Point( evt.X - selectedSticker.Rettangolo.X, evt.Y - selectedSticker.Rettangolo.Y)
        this.Invalidate()                
    
    override this.OnMouseMove(e)  =  
        if selectedSticker.Rettangolo.Height <> 0 && e.Button.Equals(MouseButtons.Left) then
            let p = wv.TransformPointV(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
            selectedSticker.Rettangolo <- new Rectangle(evt.X - lastMousePosition.X,evt.Y - lastMousePosition.Y, 100, 130)
            this.Invalidate()

    //override this.OnMouseUp(e) =

    override this.OnPaint(e) =
        let g = e.Graphics
        let bkg = e.Graphics.Save()
        let vx = wv.TransformPointV(PointF(0.f,0.f))
        
        g.FillRectangle(Brushes.White, new Rectangle(Point(0,0),Size(this.Width,this.Height)))

        g.Transform <- wv.WV

        for i in listaSticker do
            if selectedSticker <> i then
                g.FillRectangle(Brushes.YellowGreen, i.Rettangolo)
                i.OnPaint(g)
        if selectedSticker.Rettangolo.Height <> 0 then
            g.FillRectangle(Brushes.YellowGreen, selectedSticker.Rettangolo)
            g.DrawRectangle(Pens.Black, selectedSticker.Rettangolo)
            selectedSticker.OnPaint g
        g.Restore(bkg)

let mutable LWCArea = new Area() //per riferire l'area di disegno

type Buttons() =
    inherit LWCControl()

    let mutable contaRotate = 0 //indica di quanto ha ruotato il mondo fino a questo momento
    let sposta = 30.f //usato per indicare di quanto fare la translate

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
                | 0 -> printfn "←"; LWCArea.WV.TranslateV(sposta, 0.f)
                | 1 -> printfn "→"; LWCArea.WV.TranslateV(-sposta, 0.f)
                | 2 -> printfn "↑"; LWCArea.WV.TranslateV(0.f, sposta)
                | 3 -> printfn "↓"; LWCArea.WV.TranslateV(0.f, -sposta)
                | 4 -> printfn "+"; LWCArea.WV.ScaleV(1.f/float32 1.1,1.f/float32 1.1)
                | 5 -> printfn "-"; LWCArea.WV.ScaleV(1.f*float32 1.1,1.f*float32 1.1)
                | 6 -> printfn "⟲"; LWCArea.WV.RotateV(float32 -contaRotate); contaRotate <- (contaRotate-45)%360; LWCArea.WV.RotateV(float32 contaRotate)
                | 7 -> printfn "⟳"; LWCArea.WV.RotateV(float32 -contaRotate); contaRotate <- (contaRotate+45)%360; LWCArea.WV.RotateV(float32 contaRotate)
                | _ -> printfn "error"
                LWCArea.Invalidate()

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
                            LWCArea.Invalidate()
                    else 
                        MessageBox.Show("Selezionare uno sticker o limite immagini per lo sticker superato", "Impossibile selezionare file", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
                | 1 -> 
                    printfn "T"
                    if selectedSticker.Rettangolo.Height <> 0 && textBox.TextLength>0 then
                        selectedSticker.Testo <- textBox.Text
                        LWCArea.Invalidate()
                | _ -> printfn "error"
    

    override this.OnPaint(e) =
        let g = e.Graphics
        for i in 0 .. (bottoni.Length - 1) do
            g.FillRectangle(Brushes.DarkGray, bottoni.[i])
            g.DrawString(lettere.[i],new Font("Tahoma",float32 12.5),Brushes.Black,PointF(float32 bottoni.[i].X,0.f))

let f = new Form(Text="prova", TopMost=true)
f.Height <- 700
f.Width <- 700
f.BackColor <- Color.LightGray


type ScrollBarX() =
    inherit LWCControl()
    let mutable rettOrizzontale = new Rectangle(0,0,20,20)
    let mutable seleziona = false
    let mutable lastPosition = new PointF(-20.f,0.f)
    let mutable lastX = 0.f

    override this.OnMouseDown(e) =
        if checkPickCorrelationRett rettOrizzontale e.Location then
            seleziona <- true
            lastPosition <- PointF(float32 e.X - float32 rettOrizzontale.X, 0.f)

    override this.OnMouseMove(e) =
        if seleziona && e.Button.Equals(MouseButtons.Left) then
            let sottr = e.Location.X - int lastPosition.X
            let diff = float32 e.Location.X - lastX
            if not(int(this.Width) <= (rettOrizzontale.X + rettOrizzontale.Width) && diff > 0.f) && not(rettOrizzontale.X <= 0 && diff < 0.f) then
                rettOrizzontale <- new Rectangle(sottr, 0, 20, 20)
                LWCArea.WV.TranslateV(-diff,0.f)
                lastX <- float32 e.Location.X
                LWCArea.Invalidate()
                this.Invalidate()

    override this.OnMouseUp(e) =
        seleziona <- false

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Gray, 0.f, 0.f, this.Width, this.Height)
        g.FillRectangle(Brushes.Black, rettOrizzontale)

    override this.OnResize(e) =
        this.ClientSize <- SizeF(float32 (f.ClientSize.Width-20), 20.f)
        if lastPosition.X = -20.f then
            rettOrizzontale <- new Rectangle(int (this.Width/2.f),0,20,20)
            lastPosition <- new PointF(this.Width/2.f ,0.f)
            lastX <- lastPosition.X
        this.Invalidate()
        base.OnResize e

type ScrollBarY() =
    inherit LWCControl()
    let mutable rettVerticale = new Rectangle(0,0,20,20)
    let mutable seleziona = false
    let mutable lastPosition = new PointF(-20.f,0.f)
    let mutable lastY = 0.f

    override this.OnMouseDown(e) =
        if checkPickCorrelationRett rettVerticale e.Location then
            seleziona <- true
            lastPosition <- PointF(0.f, float32 e.Y - float32 rettVerticale.Y)

    override this.OnMouseMove(e) =
        if seleziona && e.Button.Equals(MouseButtons.Left) then
            let sottr = e.Location.Y - int lastPosition.Y
            let diff = float32 e.Location.Y - lastY
            if not(int(this.Height) <= (rettVerticale.Y + rettVerticale.Height) && diff > 0.f) && not(rettVerticale.Y <= 0 && diff < 0.f) then
                rettVerticale <- new Rectangle(0, sottr, 20, 20)
                LWCArea.WV.TranslateV(0.f,-diff)
                lastY <- float32 e.Location.Y
                LWCArea.Invalidate()
                this.Invalidate()

    override this.OnMouseUp(e) =
        seleziona <- false

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Gray, 0.f, 0.f, this.Width, this.Height)
        g.FillRectangle(Brushes.Black, rettVerticale)

    override this.OnResize(e) =
        this.ClientSize <- SizeF(20.f, float32 (f.ClientSize.Height-20))
        this.Position <- PointF(0.f, 0.f)
        if lastPosition.X = -20.f then
            rettVerticale <- new Rectangle(0, int (this.Height/2.f),20,20)
            lastPosition <- new PointF(0.f, this.Height/2.f)
            lastY <- lastPosition.Y
        
        this.Invalidate()
        base.OnResize e

type Disegno() =
    inherit LWCControl()
        
    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.DimGray, 0.f, 0.f, this.Width, this.Height)
        
    override this.OnResize(e) =
        this.Position <- PointF(0.f, float32 f.ClientSize.Height - 20.f)
        this.Invalidate()
        base.OnResize e


let AreaContainer = new Area(Dock=DockStyle.Fill)
let lwccBottoni = new LWCContainer(Dock=DockStyle.Top, Height=55)
let lwccScrollbarY = new LWCContainer(Dock=DockStyle.Right, Width=20)
let lwccScrollbarX = new LWCContainer(Dock=DockStyle.Bottom, Height=20)

let ButtonControl = new Buttons(Position=PointF(0.f, 0.f),ClientSize=SizeF(8.f*25.f + 5.f*7.f, 25.f))
let StickerControl = new Operazioni(Position=PointF(0.f, 30.f),ClientSize=SizeF(8.f*25.f + 5.f*7.f, 25.f))


//scrollbar
let xS = new ScrollBarX(ClientSize=SizeF(float32 (f.ClientSize.Width-20), 20.f))
let yS = new ScrollBarY(ClientSize=SizeF(20.f, float32 (f.ClientSize.Height-20)))
let cO = new Disegno(Position=PointF(0.f, float32 f.ClientSize.Height - 20.f),ClientSize=SizeF(20.f, 20.f)) //sbarra tra le due scrollbar
//------------

LWCArea <- AreaContainer

lwccScrollbarX.LWControls.Add(xS)
lwccScrollbarY.LWControls.Add(yS)
lwccScrollbarY.LWControls.Add(cO)

//lwcc.LWControls.Add(AreaDraw)
lwccBottoni.LWControls.Add(ButtonControl)
lwccBottoni.LWControls.Add(StickerControl)
lwccBottoni.Controls.Add(textBox)

f.Controls.Add(lwccBottoni)
f.Controls.Add(AreaContainer)
f.Controls.Add(lwccScrollbarX)
f.Controls.Add(lwccScrollbarY)
f.Show()