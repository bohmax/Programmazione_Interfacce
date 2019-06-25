#load "LWC.fsx"
open LWC
open System.Windows.Forms
open System.Drawing

let f = new Form(Text="prova", TopMost=true)
f.Height <- 700
f.Width <- 700
f.BackColor <- Color.LightGray

f.Show()