open System.Windows.Forms
open System.Drawing

type WVMatrix () = // Libreria
  (* data una coordinata mondo restituisce una coordinata vista
  per non sbagliare oltre a mondo-vista dovremmo poter fare anche vista-mondo, ma non tutte le matrici sono invertibili
  anche se generalmente quelle affini lo sono, ma l'inversione inoltre potrebbe accumulare instabilità numerica
  in questa costruzione c'è molto di uomo e poco di macchina, esiste perché è più semplice/utile per l'uomo  *)
  let wv = new Drawing2D.Matrix() //essendo 2D è di dimensione 3*3
  let vw = new Drawing2D.Matrix()
  member this.VW with get() = vw
  member this.WV with get() = wv

  member this.TranslateW (tx, ty) =
    wv.Translate(tx, ty)
    vw.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append) //calcola l'inversa della precedente
    (* es traslare il punto a=(0,0) di 10 lungo l’asse x:
    prima di disegnarlo nella OnPaint() dico al SistemaGrafico di moltiplicarlo per la matrice wv(10,0)
    quando mi posiziono sul punto (che ora si trova in (10,0)), prima di “leggerlo”, lo moltiplico per vw(-10,0) in modo tale che “legga” (0,0) 
    la Append funziona correttamente perché associa per prima partendo da sx
    la Prepend non avrebbe funzionato perché sarebbe andata ad associare per prima partendo da dx
    sarebbe stata in quest ultimo caso: translate = matrice corretta * matrice 1 0 0 ...
    in C sarebbe stata wv * = T(0,10) : si sta cioè moltiplicando la matrice sul posto ed è sbagliato!  
    la traslatura avviene rispetto all'origine delle coordinate  *)

  member this.ScaleW (sx, sy) =
    wv.Scale(sx, sy) //wv.Scale(1.f, -1.f), scalare di 1.1 = scalare del 10%, solitamente sx=sy altrimenti verrebbe zommato assumendo coordinate ellissi invece che cerchio
    vw.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append) //vw.Scale(1.f, -1.f, Drawing2D.MatrixOrder.Append)

  member this.RotateW (a) =
    wv.Rotate(a)
    vw.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.RotateV (a) =
    vw.Rotate(a)
    wv.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.TranslateV (tx, ty) =
    vw.Translate(tx, ty)
    wv.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleV (sx, sy) =
    vw.Scale(sx, sy)
    wv.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)
  
  member this.TransformPointV (p:PointF) = //p=coppia di punti
    let a = [| p |] //[]=lista, [| |]=array, in questo caso è un array con un solo elemento
    vw.TransformPoints(a) //trasforma un array di punti, l'array assume posizione in una matrice
    a.[0]

  member this.TransformPointW (p:PointF) = 
    let a = [| p |] 
    wv.TransformPoints(a) 
    a.[0]

 type LWCControl() =
  let wv = WVMatrix()
  let mutable pos = PointF()
  let mutable sz = SizeF(120.f, 120.f)
  let mutable parent : Control option = None

  member this.WV = wv
  member this.Left = pos.X
  member this.Top = pos.Y
  member this.Width = sz.Width
  member this.Height = sz.Height
  member this.Parent
    with get() = parent
    and set(v) = parent <- v

  member this.PositionInt with get() = Point(int pos.X, int pos.Y)
  member this.ClientSizeInt with get() = Size(int sz.Width, int sz.Height)
  member this.Position
    with get() = pos
    and set(v) =
      wv.TranslateV(pos.X, pos.Y)
      pos <- v
      wv.TranslateV(-pos.X, -pos.Y)
      this.Invalidate()
  member this.ClientSize
    with get() = sz
    and set(v) = 
      sz <- v
      this.Invalidate()
  member this.Invalidate() =
    match parent with
    | Some p -> p.Invalidate()
    | None -> ()
  member this.HitTest(p:Point) =
    let pt = wv.TransformPointV(PointF(single p.X, single p.Y))
    let boundingbox = RectangleF(0.f, 0.f, sz.Width, sz.Height)
    boundingbox.Contains(pt)

  abstract OnPaint : PaintEventArgs -> unit
  default this.OnPaint (e) = ()

  abstract OnResize : System.EventArgs -> unit
  default this.OnResize (e) = ()

  abstract OnMouseDown : MouseEventArgs -> unit
  default this.OnMouseDown (e) = ()

  abstract OnKeyDown : KeyEventArgs -> unit
  default this.OnKeyDown (e) = ()

  abstract OnMouseUp : MouseEventArgs -> unit
  default this.OnMouseUp (e) = ()

  abstract OnMouseMove : MouseEventArgs -> unit
  default this.OnMouseMove (e) = ()

  //member this.checkPickCorrelationRett (rett:Rectangle) (p:Point) = //pick correlation di un punto su un rettangolo, lasciato commentato per completezza
    //if rett.Contains p then 
        //true
    //else false

type LWCContainer() as this =
  inherit UserControl()

  let controls = System.Collections.ObjectModel.ObservableCollection<LWCControl>()

  do 
    controls.CollectionChanged.Add(fun e ->
      for i in e.NewItems do
        (i :?> LWCControl).Parent <- Some(this :> Control)
    )
    this.SetStyle(ControlStyles.DoubleBuffer, true)
    this.SetStyle(ControlStyles.AllPaintingInWmPaint, true)

  member this.LWControls with get() = controls

  override this.OnMouseDown (e) =
    let oc = controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseDown(evt)
    | None -> () 

  override this.OnKeyDown (e) =
    //let evt = new KeyEventArgs(e.KeyData)
    //this.OnKeyDown(evt)
    //controls |> Seq.iter(fun c -> c.OnKeyDown(new KeyEventArgs(e.KeyData)) )
    this.Invalidate()


  override this.OnMouseUp (e) =
    let oc = controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseUp(evt)
    | None -> () 

  override this.OnMouseMove (e) =
    let oc =  controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseMove(evt)
    | None -> () 

  override this.OnResize(e) =
    controls |> Seq.iter(fun c -> c.OnResize(e) )

    //this.ClientSize <- SizeF(float32(e.ClientSize.Width), float32(this.ClientSize.Height))
    //let evt = new 
    //c.OnMouseMove(evt)

  override this.OnPaint(e) =
    let g = e.Graphics
    g.SmoothingMode <- System.Drawing.Drawing2D.SmoothingMode.AntiAlias
    controls 
    |> Seq.iter(fun c ->
      let bkg = e.Graphics.Save()

      // esercizio: impostare il rettangolo in client space
      let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt, c.ClientSizeInt))
      //bug: non supporta la rotazione
      e.Graphics.SetClip(new RectangleF(c.Position, c.ClientSize))
      e.Graphics.Transform <- c.WV.WV
      c.OnPaint(evt)
      e.Graphics.Restore(bkg)
    )

// Utente Libreria
type LWButton() =
  inherit LWCControl()
  
  override this.OnPaint(e) =
    let g = e.Graphics
    g.FillRectangle(Brushes.Red, 0.f, 0.f, this.Width, this.Height)
    g.DrawLine(Pens.Blue, 0.f, 0.f, 2.f*this.Width, 2.f*this.Height)
  
  override this.OnMouseDown(e) =
    printfn "%A" e.Location

// Test
//let btn = new LWButton(Position=PointF(20.f, 10.f))
//let btn2 = new LWButton(Position=PointF(180.f, 10.f))

//let lwcc = new LWCContainer(Dock=DockStyle.Fill)
//lwcc.LWControls.Add(btn)
//lwcc.LWControls.Add(btn2)

//let f = new Form(Text="Prova", TopMost=true)
//f.Controls.Add(lwcc)
//f.Show()

// btn.Position <- PointF(150.f, 70.f)
// lwcc.Invalidate()