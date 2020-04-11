// Copyright (c) 2019, The Emergent Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"github.com/emer/gospeech/en/phoeticparse"
	"github.com/emer/gospeech/en/textparse"
	"github.com/emer/gospeech/v2"
	"github.com/goki/gi/gi"
	"github.com/goki/gi/gimain"
	"github.com/goki/gi/giv"
	"github.com/goki/ki/ki"
	"golang.org/x/exp/errors/fmt"
)

// this is the stub main for gogi that calls our actual
// mainrun function, at end of file
func main() {
	gimain.Main(func() {
		mainrun()
	})
}

// Synth encapsulates
type Synth struct {
	win            *gi.Window
	ToolBar        *gi.ToolBar `view:"-" desc:"the master toolbar"`
	ModelConfig    v2.ModelConfig
	TrmConfig      v2.TrmConfig
	Text           string `desc:"the text to be synthesized"`
	Phonetic       string `desc:"the phonetic version of Text"`
	TextParser     *textparse.TextParser
	PhoneticParser *phoneticparse.PhoneticParser
	Model          *v2.Model
	Control        *v2.Control
}

func (syn *Synth) Defaults() {
	// syn.SignalData = &etable.Table{}
	// syn.ConfigSignalData(syn.SignalData)
	// syn.Save = false
	// syn.Play = false
}

func (syn *Synth) Config() {
	syn.ModelConfig.Load("trmControl.json")
	vfp := "./voice_" + syn.ModelConfig.Voice + ".json"
	syn.TrmConfig.Load("trm.json", vfp)

	syn.Model = v2.NewModel("../../data/en/monet_go.xml")
	syn.Control = v2.NewControl("", syn.Model)

	syn.TextParser = textparse.NewTextParser()
	syn.Text = "emergent"

	syn.PhoneticParser = phoneticparse.NewPhoneticParser(syn.Model, syn.Control, "../../data/en/vowelTransitions")
}

func (syn *Synth) Synthesize() {
	if len(syn.Text) == 0 {
		gi.PromptDialog(syn.win.Viewport, gi.DlgOpts{Title: "No text to synthesize", Prompt: fmt.Sprintf("Enter the text to synthesize in the Text field.")}, gi.AddOk, gi.NoCancel, nil, nil)
		return
	}

	syn.Phonetic = syn.TextParser.ParseText(syn.Text)
	fmt.Println(syn.Text, "phoneticaly is ", syn.Phonetic)
	syn.PhoneticParser = phoneticparse.NewPhoneticParser(syn.Model, syn.Control, "../../data/en/")
	syn.Control.SynthPhoneticStringToFile(syn.PhoneticParser, syn.Phonetic, "trmParams.txt", "out.txt")
	//syn.Control.SynthPhoneticStringToFile(syn.PhoneticParser, "/c // # /w /l i./*m_er_r.j_uh_n_t # // /c", "trmParams.txt", "out.txt")
}

////////////////////////////////////////////////////////////////////////////////////////////
// 		Gui

// ConfigGui configures the GoGi gui interface for this Aud
func (syn *Synth) ConfigGui() *gi.Window {
	width := 1600
	height := 1200

	gi.SetAppName("Synth")
	gi.SetAppAbout(`This demonstrates synthesizing a sound (phone or word)`)

	win := gi.NewMainWindow("one", "Auditory ...", width, height)

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

	// tview := gi.AddNewTabView(split, "tv")

	// plt := tview.AddNewTab(eplot.KiT_Plot2D, "wave").(*eplot.Plot2D)
	// syn.WavePlot = syn.ConfigWavePlot(plt, syn.SignalData)

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
	Synther.Defaults()
	// Synther.vt.Init()
	Synther.Config()
	Synther.win = Synther.ConfigGui()
	Synther.win.StartEventLoop()
}
