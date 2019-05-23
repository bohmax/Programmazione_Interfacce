#load "LWC.fsx"
open LWC

open System.Windows.Forms
open System.Drawing

let f = new Form(Text="prova", TopMost=true)

type Rettangoli() =
    inherit LWCControl()

    override this.OnResize(e) =
        this.ClientSize <- SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height))
        this.Invalidate()
        base.OnResize e


let lwcc = new LWCContainer(Dock=DockStyle.Fill)
let r = new Rettangoli(ClientSize=SizeF(float32(f.ClientSize.Width), float32(f.ClientSize.Height)))
lwcc.LWControls.Add(r)

f.Controls.Add(lwcc)

f.Show()