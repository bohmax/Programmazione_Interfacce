open System.Windows.Forms
open System.Drawing

let f = new Form( Text="orologio", TopMost=true )
let mutable tempo = System.DateTime.Now
f.Show()

type Circonferenza ( rectx, recty, larghezza, altezza) = 
    let mutable rx = rectx  //mutable si usa per indicare variabili non fisse
    let mutable ry = recty
    let mutable rb = larghezza
    let mutable rh = altezza
    let mutable raggio = rb/2
    let rectangle = Rectangle(rx,ry,rb,rh)
    member this.RX with get() = rx and set(value) = rx <- value //member si usa per dichiarare i metodi all'interno della classe
    member this.RY with get() = ry and set(value) = ry <- value //ogni memeber chiama il nome dell'oggetto con this
    member this.RB with get() = rb and set(value) = rb <- value
    member this.RH with get() = rh and set(value) = rh <- value
    member this.Rectangle with get() = rectangle
    member this.RAGGIO with get() = raggio and set(value) = raggio <- value

type WVCoordinate() =
    let wv = new Drawing2D.Matrix()
    let vw = new Drawing2D.Matrix()
    member this.VW with get() = vw
    member this.WV with get() = wv

    member this.Rotate( angle ) = 
        wv.Rotate(angle)
        vw.Rotate(-angle, Drawing2D.MatrixOrder.Append) 

    member this.Translate( rx, ry ) =
        wv.Translate( rx, ry)
        vw.Translate( -rx, -ry, Drawing2D.MatrixOrder.Append)

type clock() as this =
    inherit UserControl()
    let mutable buf = new Bitmap(this.Width, this.Height)

    override this.OnPaint e = 
        let g = e.Graphics
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality //antialiasing: tutto disegnato in modo che l'effetto quadratato dei pixel appaia più lineare
        this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)

        let cerchio = new Circonferenza(this.ClientSize.Width/2-50,this.ClientSize.Height/2-50, 100, 100)
        //g.DrawEllipse(Pens.Black, f.ClientSize.Width/2-50, f.ClientSize.Height/2-50, 100, 100) //rettangolo grande 100*100 la cui origine parte spostata della metà che è 50
        g.DrawEllipse(Pens.Black, cerchio.Rectangle)

        //g.DrawLine(Pens.Blue, f.ClientSize.Width/2, f.ClientSize.Height/2, f.ClientSize.Width/2, f.ClientSize.Height/2-50) //qui sono le coordinate del punto iniziale e finale
        //g.DrawLine(Pens.Blue, cerchio.RX+50, cerchio.RY+50, cerchio.RX+50, cerchio.RY)

        let coordinate = new WVCoordinate()
        coordinate.Translate( single(cerchio.RX+cerchio.RAGGIO), single(cerchio.RY+cerchio.RAGGIO))
        for i in 1..12 do
            //let t = g.Transform
            //g.DrawLine(Pens.Black, cerchio.RX+50, cerchio.RY, cerchio.RX+50, cerchio.RY+10)
            coordinate.Rotate(float32(30))  
            g.Transform <- coordinate.WV
            g.DrawLine(Pens.Black, 0, cerchio.RAGGIO, 0, 40) //qui sono le coordinate del punto iniziale e finale

        //coordinate.Translate( -single(cerchio.RX+cerchio.RAGGIO), -single(cerchio.RY+cerchio.RAGGIO)) //rispostiamo il mondo alle sue coordinate standard
        //g.Transform <- coordinate.WV //salviamo le modifiche
        //g.DrawLine(Pens.Blue, 0, 0, 100, 100) //così vediamo quello che sarebbe successo
        //g.DrawLine(Pens.Black, cerchio.RX+50, cerchio.RY+50, cerchio.RX+50, cerchio.RY)

        coordinate.Rotate(float32(180))  
        //g.Transform <- coordinate.WV

        let ora = tempo.Hour%12 //il modulo non è fondamentale perché essendo in gradi moltiplica ciclicamente
        let minuti = tempo.Minute
        let secondi = tempo.Second

        //printfn "%d" ora 
        coordinate.Rotate(float32(30*ora+minuti/2))
        g.Transform <- coordinate.WV //qui è importante perché prima di disegnare devi spostarti
        g.DrawLine(Pens.Black, 0, 0, 0, cerchio.RAGGIO)
        coordinate.Rotate(-float32(30*ora+minuti/2))
        //g.Transform <- coordinate.WV

        coordinate.Rotate(float32(6*minuti))
        g.Transform <- coordinate.WV
        g.DrawLine(Pens.Blue, 0, 0, 0, cerchio.RAGGIO-10)
        coordinate.Rotate(-float32(6*minuti))

        coordinate.Rotate(float32(6*secondi))
        g.Transform <- coordinate.WV
        g.DrawLine(Pens.Aqua, 0, 0, 0, cerchio.RAGGIO-20)
        coordinate.Rotate(-float32(6*secondi))
        g.Transform <- coordinate.WV
    
        //base.OnPaint e //a cosa serve?
        f.Invalidate()

    override this.OnResize e = //quando si effettua la resize della form deve poter ridisegnare
        this.Invalidate()
        base.OnResize e


let orologio = new clock(Dock=DockStyle.Fill) //Dock=DockStyle.Fill specifica la modalità di ancoraggio di un controllo, altrimenti non verrebbe posizionato al centro come specificato
f.Controls.Add(orologio)

let t = new Timer(Interval=950)

//siccome la coda degli eventi non esegue tutto nell'immediato ma passa un po' di tempo prima che gestisca l'evento
//dobbiamo dargli il tempo affinché lo possa fare
t.Tick.Add( fun e ->
    tempo <- System.DateTime.Now
    orologio.Invalidate()
)
t.Start()