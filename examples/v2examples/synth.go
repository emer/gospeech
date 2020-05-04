// Copyright (c) 2019, The Emergent Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"strconv"

	"github.com/emer/etable/eplot"
	"github.com/emer/etable/etable"
	"github.com/emer/etable/etensor"
	"github.com/emer/gospeech/en/phoeticparse"
	"github.com/emer/gospeech/en/textparse"
	"github.com/emer/gospeech/v2"
	"github.com/goki/gi/gi"
	"github.com/goki/gi/gimain"
	"github.com/goki/gi/giv"
	"github.com/goki/ki/ki"
	"golang.org/x/exp/errors/fmt"

	_ "github.com/emer/etable/etview" // include to get gui views
)

const intonationPath = "../../data/en/intonation"

// this is the stub main for gogi that calls our actual
// mainrun function, at end of file
func main() {
	gimain.Main(func() {
		mainrun()
	})
}

// Synth encapsulates
type Synth struct {
	win            *gi.Window                    `view:"-" desc:"main window"`
	StructView     *giv.StructView               `view:"-" desc:"the params viewer"`
	ToolBar        *gi.ToolBar                   `view:"-" desc:"the master toolbar"`
	Text           string                        `desc:"the text to be synthesized"`
	Phonetic       string                        `desc:"the phonetic version of Text"`
	TextParser     *textparse.TextParser         `view:"-" desc:parses text, returns phonetic string"`
	PhoneticParser *phoneticparse.PhoneticParser `view:"-" desc:"parses the phonetic string"`
	Model          *v2.Model                     `view:"" desc:"the master toolbar"`
	Control        *v2.Control                   `view:"+" desc:"the master toolbar"`
	SignalData     *etable.Table                 `desc:"waveform data"`
	WavePlot       *eplot.Plot2D                 `view:"-" desc:"waveform plot"`
	Save           bool                          `desc:"if true write the synthesized values to .wav file"`
	Play           bool                          `desc:"if true play the sound"`
}

func NewSynth() *Synth {
	syn := new(Synth)
	syn.SignalData = &etable.Table{}
	syn.ConfigSignalData(syn.SignalData)
	syn.Save = false
	syn.Play = false

	syn.Model = v2.LoadModel("../../data/en/monet_go.xml")
	syn.Control = v2.NewControl(intonationPath, syn.Model)

	syn.Control.ModelConfig.Load("trmControl.json")
	vfp := "./voice_" + syn.Control.ModelConfig.Voice + ".json"
	syn.Control.TrmConfig.Load("trm.json", vfp)

	syn.TextParser = textparse.NewTextParser()
	syn.Text = "emergent"

	syn.PhoneticParser = phoneticparse.NewPhoneticParser(syn.Control, "../../data/en/vowelTransitions")
	return syn
}

// ConfigSignalData
func (syn *Synth) ConfigSignalData(dt *etable.Table) {
	dt.SetMetaData("name", "Wave")
	dt.SetMetaData("desc", "Waveform values -1 to 1")
	dt.SetMetaData("read-only", "true")
	dt.SetMetaData("precision", strconv.Itoa(4))

	sch := etable.Schema{
		{"Time", etensor.FLOAT64, nil, nil},
		{"Amplitude", etensor.FLOAT64, nil, nil},
	}
	dt.SetFromSchema(sch, 0)
}

func (syn *Synth) ConfigWavePlot(plt *eplot.Plot2D, dt *etable.Table) *eplot.Plot2D {
	plt.Params.Title = "Waveform plot"
	plt.Params.XAxisCol = "Time"
	plt.SetTable(dt)

	// order of params: on, fixMin, min, fixMax, max
	plt.SetColParams("Amplitude", eplot.On, eplot.FixMin, -1, eplot.FloatMax, 1)

	return plt
}

func (syn *Synth) GetWaveData() {
	syn.SignalData.AddRows(len(syn.Control.Tube.SynthOutput))
	for i := 0; i < len(syn.Control.Tube.SynthOutput); i++ {
		syn.SignalData.SetCellFloat("Time", i, float64(i))
		syn.SignalData.SetCellFloat("Amplitude", i, float64(syn.Control.Tube.Wave[i]))
	}
}

func (syn *Synth) Synthesize() {
	if len(syn.Text) == 0 {
		gi.PromptDialog(syn.win.Viewport, gi.DlgOpts{Title: "No text to synthesize", Prompt: fmt.Sprintf("Enter the text to synthesize in the Text field.")}, gi.AddOk, gi.NoCancel, nil, nil)
		return
	}

	syn.Phonetic = syn.TextParser.ParseText(syn.Text)
	fmt.Println(syn.Text, "phoneticaly is ", syn.Phonetic)
	if syn.StructView != nil {
		syn.StructView.UpdateField("Phonetic")
	}

	syn.Control.SynthPhoneticStringToFile(syn.PhoneticParser, syn.Phonetic, "trmParams.txt", "out.txt")
	syn.GetWaveData()
	syn.WavePlot.GoUpdate()
	if syn.Save {
		fn := syn.Text + ".wav"
		err := syn.Control.Tube.Buf.WriteWave(fn)
		if err != nil {
			fmt.Printf("File not found or error opening file: %s (%s)", fn, err)
		}
	}
}

////////////////////////////////////////////////////////////////////////////////////////////
// 		Gui

// ConfigGui configures the GoGi gui interface for this Aud
func (syn *Synth) ConfigGui() *gi.Window {
	width := 1600
	height := 1200

	gi.SetAppName("Synth")
	gi.SetAppAbout(`This program calls gospeech (a port of gnuspeech to synthesize text.`)

	win := gi.NewMainWindow("Synth Two", "Auditory ...", width, height)

	vp := win.WinViewport2D()
	updt := vp.UpdateStart()

	mfr := win.SetMainFrame()

	tbar := gi.AddNewToolBar(mfr, "tbar")
	tbar.SetStretchMaxWidth()
	syn.ToolBar = tbar

	split := gi.AddNewSplitView(mfr, "split")
	split.Dim = gi.X
	split.SetStretchMax()

	sv := giv.AddNewStructView(split, "sv")
	sv.SetStruct(syn)
	syn.StructView = sv

	tview := gi.AddNewTabView(split, "tv")

	plt := tview.AddNewTab(eplot.KiT_Plot2D, "wave").(*eplot.Plot2D)
	syn.WavePlot = syn.ConfigWavePlot(plt, syn.SignalData)

	// tbar.AddAction(gi.ActOpts{Label: "Update Wave", Icon: "new"}, win.This(),
	// 	func(recv, send ki.Ki, sig int64, data interface{}) {
	// 		syn.GetWaveData()
	// 	})

	tbar.AddAction(gi.ActOpts{Label: "Synthesize", Icon: "new"}, win.This(),
		func(recv, send ki.Ki, sig int64, data interface{}) {
			syn.Synthesize()
		})

	split.SetSplitsList([]float32{.3, .7})

	// main menu
	appnm := gi.AppName()
	mmen := win.MainMenu
	mmen.ConfigMenus([]string{appnm, "File", "Edit", "Window"})

	amen := win.MainMenu.ChildByName(appnm, 0).(*gi.Action)
	amen.Menu.AddAppMenu(win)

	emen := win.MainMenu.ChildByName("Edit", 1).(*gi.Action)
	emen.Menu.AddCopyCutPaste(win)

	vp.UpdateEndNoSig(updt)

	win.MainMenuUpdated()
	return win
}

var Synther Synth

func mainrun() {
	Synther = *NewSynth()
	Synther.win = Synther.ConfigGui()
	Synther.win.StartEventLoop()
}
