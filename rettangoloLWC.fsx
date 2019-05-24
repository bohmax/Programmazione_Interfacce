#load "LWC.fsx"
open LWC

open System.Windows.Forms
open System.Drawing

let f = new Form(Text="MovingBox", TopMost=true)

type Rettangolo() =
    inherit LWCControl()
    let mutable press : bool = false
    let mutable rett = new Rectangle(0, 0, 30, 30)

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.Red, rett)

    override this.OnMouseDown(e) =
        press <- true

    override this.OnMouseUp(e) =
        press <- false

    override this.OnMouseMove(e) =
        if press then
            rett <- new Rectangle(e.Location.X, e.Location.Y, 30, 30)
            this.Invalidate() 

    override this.OnResize(e) =
        this.ClientSize <- SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height))

        this.Invalidate()
        base.OnResize e



let lwcc = new LWCContainer(Dock=DockStyle.Fill)
let r = new Rettangolo(ClientSize=SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height)))
lwcc.LWControls.Add(r)

f.Controls.Add(lwcc)
f.Show()