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
let rec trovaSticker (l:Sticker list) (e:Point) =  //trova se stiamo cliccando in uno degli archi della lista
    match l with
    | [] -> new Sticker()
    | head :: tail -> 
    if not ( head.Rettangolo.Contains e) then 
        trovaSticker tail e
    else head

let calcolaDiff ( el: Sticker ) (selected: Sticker ) =
    let mutable diffX = el.Position.X - selected.Position.X
    let mutable diffY = el.Position.Y - selected.Position.Y
    let mutable x = 0.f
    let mutable y = 0.f
    if abs diffX < 35.f then 
        x <- selected.Position.X
    else if abs diffX < 75.f then 
        x <- el.Position.X - 5.f * float32( sign diffX )
    else
        x <- el.Position.X - 10.f * float32( sign diffX )

    if abs diffY < 35.f then 
        y <- selected.Position.Y
    else if abs diffY < 75.f then 
        y <- el.Position.Y -  5.f * float32( sign diffY )
    else
        y <- el.Position.Y -  10.f * float32( sign diffY )
    PointF(x, y)
//-----------------------------------------------------------------------------------------

let mutable listaSticker = [] //indica una lista degli sticker
let mutable selectedSticker = new Sticker()
let mutable posMousePosition = Point(-1,-1) //ultima posizione del mouse dell evento onmousedown
let mutable contaRotate = 0 //indica di quanto ha ruotato il mondo fino a questo momento
let mutable abilitaTrascinamento = false //per sapere se applicare la selezione stile lasso
let mutable drawSelector = new Rectangle(0,0,0,0) //rettangolo di selezione da disegnare
let t = new Timer(Interval = 20)
    
type Area() =
    inherit LWCContainer()

    let wv = WVMatrix()

    member this.WV
        with get() = wv
    
    override this.OnMouseDown(e) =
        if not t.Enabled then
            let p = wv.TransformPointV(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
        
            let risultatoricerca = trovaSticker listaSticker evt.Location
            if risultatoricerca.Rettangolo.Height = 0 then //aggiungi nuovo sticker
                abilitaTrascinamento <- true
                posMousePosition <- Point( evt.X, evt.Y)
            else //seleziona lo steaker cliccato
                selectedSticker.Selezionato <- false
                selectedSticker <- risultatoricerca

                listaSticker <- List.append [selectedSticker] listaSticker //in modo da avere l'elemento selezionato in cima alla lista
                listaSticker <- List.distinct listaSticker //elimino il duplicato dopo l'inserimento
                let idx = this.LWControls.IndexOf(selectedSticker)
                this.LWControls.Move(idx,this.LWControls.Count-1)

                selectedSticker.Selezionato <- true
                posMousePosition <- Point( evt.X - int selectedSticker.Position.X, evt.Y - int selectedSticker.Position.Y)
                abilitaTrascinamento <- false
            for i in listaSticker do
                if i <> selectedSticker then
                    i.Selezionato <- false
            this.Invalidate()              
    
    override this.OnMouseMove(e)  =
        if not t.Enabled then
            if e.Button.Equals(MouseButtons.Left) then
                let p = wv.TransformPointV(PointF(single e.X, single e.Y))
                let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)

                if selectedSticker.Rettangolo.Height <> 0 && not abilitaTrascinamento then
                    selectedSticker.Position <- new PointF(float32(evt.X - posMousePosition.X),float32(evt.Y - posMousePosition.Y))
                    selectedSticker.Rettangolo <- new Rectangle(int selectedSticker.Position.X,int selectedSticker.Position.Y,selectedSticker.Rettangolo.Width,selectedSticker.Rettangolo.Height)
                    this.Invalidate()
                else if abilitaTrascinamento then
                    let mutable (startx, sottrx) = evt.X, posMousePosition.X - evt.X
                    let mutable (starty, sottry) = evt.Y, posMousePosition.Y - evt.Y
                    if posMousePosition.X < evt.X then
                        startx <- posMousePosition.X
                        sottrx <- evt.X - posMousePosition.X
                    if posMousePosition.Y < starty then
                        starty <- posMousePosition.Y
                        sottry <- evt.Y - posMousePosition.Y

                    drawSelector <- new Rectangle(startx,starty,sottrx,sottry)
                    for i in listaSticker do
                        if drawSelector.IntersectsWith i.Rettangolo then
                            if not i.Selezionato then
                                i.Selezionato <- true
                                let idx = this.LWControls.IndexOf(i)
                                this.LWControls.Move(idx,this.LWControls.Count-1)
                        else if i <> selectedSticker then
                            i.Selezionato <- false
                    if selectedSticker.Rettangolo.Height <> 0 then
                        let idx = this.LWControls.IndexOf(selectedSticker)
                        this.LWControls.Move(idx,this.LWControls.Count-1)
                    this.Invalidate()
            else abilitaTrascinamento <- false

    override this.OnMouseUp(e)  =
        if not t.Enabled then
            let p = wv.TransformPointV(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
        
            if abilitaTrascinamento then
                if drawSelector.Width < 5 then //quando era selezionato lo sticker
                    selectedSticker.Selezionato <- false
                    let st = new Sticker(Position=PointF(float32 evt.Location.X,float32 evt.Location.Y), ClientSize=SizeF(100.f,130.f),Rettangolo = new Rectangle(evt.Location.X,evt.Location.Y,100,130))
                    listaSticker <- List.append [ st ] listaSticker
                    this.LWControls.Add(st)
                    selectedSticker <- st
                    selectedSticker.Selezionato <- true
                    drawSelector <- new Rectangle(0,0,0,0)
                else //inizio animazione Sticker selezionati
                    drawSelector <- new Rectangle(0,0,0,0)
                    t.Start()
                this.Invalidate()
            abilitaTrascinamento <- false

    override this.PaintSupport(e) =
        let g = e.Graphics
        let bkg = e.Graphics.Save()

        g.FillRectangle(Brushes.White, new Rectangle(Point(0,0),Size(this.Width,this.Height)))

        g.Transform <- wv.WV

        wv, bkg

    override this.drawSelezione(e) =
        e.DrawRectangle(Pens.Black, drawSelector)

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
                | 5 -> printfn "-"; LWCArea.WV.ScaleV(float32 1.1,float32 1.1)
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

let f = new Form(Text="prova")
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
        if rettOrizzontale.Contains e.Location then
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
        if rettOrizzontale.X+20 > int this.ClientSize.Width then
            rettOrizzontale <- new Rectangle(int this.ClientSize.Width - 20,0, 20,20)
            lastX <- this.ClientSize.Width - 20.f
        this.Invalidate()
        base.OnResize e

type ScrollBarY() =
    inherit LWCControl()
    let mutable rettVerticale = new Rectangle(0,0,20,20)
    let mutable seleziona = false
    let mutable lastPosition = new PointF(-20.f,0.f)
    let mutable lastY = 0.f

    override this.OnMouseDown(e) =
        if rettVerticale.Contains e.Location then
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
        if lastPosition.X = -20.f then //per la prima resize
            rettVerticale <- new Rectangle(0, int (this.Height/2.f),20,20)
            lastPosition <- new PointF(0.f, this.Height/2.f)
            lastY <- lastPosition.Y
        if rettVerticale.Y+20 > int this.ClientSize.Height then
            rettVerticale <- new Rectangle(0,int this.ClientSize.Height - 20, 20,20)
            lastY <- this.ClientSize.Height - 20.f
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

f.FormClosing.Add(fun _ ->
    t.Stop()
)

t.Tick.Add( fun e ->
    let mutable numselezionati = 0
    for i in listaSticker do
        if i.Selezionato && i <> selectedSticker then
            let pos = calcolaDiff i selectedSticker 
            i.Position <- PointF(pos.X, pos.Y)
            if pos.X = selectedSticker.Position.X && pos.Y = selectedSticker.Position.Y then
                i.Selezionato <- false
            else numselezionati <- numselezionati + 1
    if numselezionati = 0 then 
        t.Stop()
    LWCArea.Invalidate()
)