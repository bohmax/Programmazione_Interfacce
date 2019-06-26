#load "LWC.fsx"
open LWC
open System.Windows.Forms
open System.Drawing

type Sticker() =
    inherit LWCControl()

    let mutable rettangolo = new Rectangle(-1,-1,0,0)
    let mutable ruota = 0.f
    let mutable ruotaprec = 0.f
    let mutable testo = ""
    let mutable font = new Font("Arial", 12.f, FontStyle.Bold)
    let mutable image = []
    let mutable selezionato = false
    
    member this.Rettangolo 
        with get() = rettangolo
        and set(v) = rettangolo <- v
    member this.Ruota
        with get() = ruota
        and set(v) = ruota <- v
    member this.Testo 
        with get() = testo
        and set(v) = testo <- v
    member this.Font 
        with get() = font
        and set(v) = font <- v
    member this.Image 
        with get() = image
        and set(v) = image <- v
    member this.Selezionato
        with get() = selezionato
        and set(v) = selezionato <- v

    override this.OnPaint(e) = 
        let mutable count = 0
        let g = e.Graphics

        //printfn "%A" (abs ruota * this.Width/2.f)
        if (ruota <> 0.f) then
            this.WV.TranslateW(this.Width/2.f,this.Height/2.f)
            this.WV.RotateW(ruota)
            this.WV.TranslateW(-this.Width/2.f,-this.Height/2.f)

        let div = abs (int (ruota / 90.f))
        match div with
        | 1 | 3 -> rettangolo <- new Rectangle((int this.Position.X - 15), (int this.Position.Y + 15),130,100)
        | _ -> rettangolo <- new Rectangle(int this.Position.X,int this.Position.Y,100,130)

        g.Transform <- this.WV.WV

        let rect = new Rectangle(Point(0,0),Size(100,130))
        g.FillRectangle(Brushes.YellowGreen, rect)
        if selezionato then
            g.DrawRectangle(Pens.Black, rect)

        for i in image do
            g.DrawImage(i, new Rectangle(5,30*count+5,100-10,30))
            count <- count + 1
        let mutable dimTesto = testo.Length
        let mutable posStringa = 0
        let mutable contaAltezza = 30*count + 5
        let mutable todraw = testo

        let mutable size = TextRenderer.MeasureText(testo, font); //studio la dimensione del testo in anticipo

        while size.Width <> 0 && (contaAltezza + size.Height - 5) < (rettangolo.Height + rettangolo.Y) && posStringa < testo.Length do

            let mutable numlettere = 0 //numero lettere usate per disegnare
            
            while numlettere < (dimTesto - posStringa) && TextRenderer.MeasureText(testo.Substring( posStringa , numlettere), font).Width < rettangolo.Width - 5 do
                numlettere <- numlettere + 1

            todraw <- testo.Substring( posStringa , numlettere)
            g.DrawString(todraw,font, Brushes.Black, PointF(float32(-2.f), float32(contaAltezza)))
            size <- TextRenderer.MeasureText(todraw,font)
            contaAltezza <- contaAltezza + size.Height
            posStringa <- posStringa + numlettere
        if (ruota <> 0.f) then
            this.WV.TranslateW(this.Width/2.f,this.Height/2.f)
            this.WV.RotateW(-ruota)
            this.WV.TranslateW(-this.Width/2.f,-this.Height/2.f)
 
//------------------------------------funzioni di utilita----------------------------------
let checkPickCorrelationRett (rett:Rectangle) (p:Point) = //pick correlation di un punto su un rettangolo
    if rett.Contains p then 
        true
    else false

let rec trovaSticker (l:Sticker list) (e:Point) =  //trova se stiamo cliccando in uno degli archi della lista
    match l with
    | [] -> new Sticker()
    | head :: tail -> 
    if not ( checkPickCorrelationRett head.Rettangolo e) then 
        trovaSticker tail e
    else head
//-----------------------------------------------------------------------------------------

let mutable listaSticker = [] //indica una lista degli sticker
let mutable selectedSticker = new Sticker()
let mutable lastMousePosition = Point(-1,-1)
let mutable contaRotate = 0 //indica di quanto ha ruotato il mondo fino a questo momento

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
            selectedSticker.Selezionato <- false
            let st = new Sticker(Position=PointF(float32 evt.Location.X,float32 evt.Location.Y), ClientSize=SizeF(100.f,130.f),Rettangolo = new Rectangle(evt.Location.X,evt.Location.Y,100,130))
            listaSticker <- List.append [ st ] listaSticker
            this.LWControls.Add(st)
            selectedSticker <- st
            selectedSticker.Selezionato <- true
        else //seleziona lo steaker cliccato
            selectedSticker.Selezionato <- false
            selectedSticker <- risultatoricerca
            //listaSticker <- List.append [selectedSticker] listaSticker //in modo da avere l'elemento selezionato in cima alla lista
            //listaSticker <- List.distinct listaSticker //elimino il duplicato dopo l'inserimento
            let idx = this.LWControls.IndexOf(selectedSticker)
            this.LWControls.Move(idx,this.LWControls.Count-1)
            selectedSticker.Selezionato <- true
        lastMousePosition <- Point( evt.X - int selectedSticker.Position.X, evt.Y - int selectedSticker.Position.Y)
        this.Invalidate()                
    
    override this.OnMouseMove(e)  =  
        if selectedSticker.Rettangolo.Height <> 0 && e.Button.Equals(MouseButtons.Left) then
            let p = wv.TransformPointV(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)

            selectedSticker.Position <- new PointF(float32(evt.X - lastMousePosition.X),float32(evt.Y - lastMousePosition.Y))
            selectedSticker.Rettangolo <- new Rectangle(int selectedSticker.Position.X,int selectedSticker.Position.Y,selectedSticker.Rettangolo.Width,selectedSticker.Rettangolo.Height)
            this.Invalidate()

    override this.PaintSupport(e) =
        let g = e.Graphics
        let bkg = e.Graphics.Save()

        g.FillRectangle(Brushes.White, new Rectangle(Point(0,0),Size(this.Width,this.Height)))

        g.Transform <- wv.WV

        wv, bkg

    override this.OnResize(e) =
        base.OnResize e
        this.Invalidate()

let mutable LWCArea = new Area() //per riferire l'area di disegno

type Buttons() =
    inherit LWCControl()

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
                | 6 -> printfn "⟲"; LWCArea.WV.RotateV(float32 -contaRotate); contaRotate <- (contaRotate+30)%360; LWCArea.WV.RotateV(float32 contaRotate)
                | 7 -> printfn "⟳"; LWCArea.WV.RotateV(float32 -contaRotate); contaRotate <- (contaRotate-30)%360; LWCArea.WV.RotateV(float32 contaRotate)
                | _ -> printfn "error"
                LWCArea.Invalidate()

    override this.OnPaint(e) =
        let g = e.Graphics
        for i in 0 .. (bottoni.Length - 1) do
            g.FillRectangle(Brushes.DarkGray, bottoni.[i])
            g.DrawString(lettere.[i],new Font("Tahoma",float32 12.5),Brushes.Black,PointF(float32 bottoni.[i].X,0.f))

//let textBox = new TextBox(Top=30, Left=60, Width=8*25 + 5*7 - 60)
let textBox = new TextBox(Top=30, Left=60, Width=25*5+5*5 - 35)

type Operazioni() =
    inherit LWCControl()

    let bottoni = [| new Rectangle(0,0,25,25); new Rectangle(25+5,0,25,25); new Rectangle(25*6+5*6,0,25,25); new Rectangle(25*7+5*7,0,25,25) |]
    let lettere = [| "📄";"T";"⟲";"⟳" |]

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
                | 2 -> 
                    printfn "⟲"
                    if selectedSticker.Rettangolo.Height <> 0 then
                        selectedSticker.Ruota <- (selectedSticker.Ruota - 90.f)%360.f
                        LWCArea.Invalidate()
                | 3 -> 
                    printfn "⟳"
                    if selectedSticker.Rettangolo.Height <> 0 then
                        selectedSticker.Ruota <- (selectedSticker.Ruota + 90.f)%360.f
                        LWCArea.Invalidate()
                | _ -> printfn "error"
    

    override this.OnPaint(e) =
        let g = e.Graphics
        for i in 0 .. bottoni.Length - 1 do
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