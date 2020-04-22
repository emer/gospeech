// Copyright (c) 2020, The Emergent Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/***************************************************************************
 *  Copyright 1991, 1992, 1993, 1994, 1995, 1996, 2001, 2002               *
 *    David R. Hill, Leonard Manzara, Craig Schock                         *
 *                                                                         *
 *  This program is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by   *
 *  the Free Software Foundation, either version 3 of the License, or      *
 *  (at your option) any later version.                                    *
 *                                                                         *
 *  This program is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of         *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *
 *  GNU General Public License for more details.                           *
 *                                                                         *
 *  You should have received a copy of the GNU General Public License      *
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.  *
 ***************************************************************************/
// 2014-09
// This file was copied from Gnuspeech and modified by Marcelo Y. Matuda.

// 2019-02
// This is a port to golang of the C++ Gnuspeech port by Marcelo Y. Matuda

package v2

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"

	"github.com/goki/ki/bitflag"
)

type Control struct {
	Model       *Model
	Sequence    *Sequence
	ModelConfig ModelConfig
	TrmConfig   TrmConfig
}

func NewControl(path string, model *Model) *Control {
	ctrl := Control{}
	ctrl.Model = model
	ctrl.Sequence = NewSequence(path, model)
	return &ctrl
}

//SynthSequenceToFile synthesizes speech from data contained in the event list
func (ctrl *Control) SynthSequenceToFile(trmParamFile, outputFile string) {
	f, err := os.Create(trmParamFile)
	if err != nil {
		log.Printf("Error trying to open %v\n", outputFile)
		return
	}
	writer := bufio.NewWriter(f)
	ctrl.InitUtterance(writer)
	ctrl.Sequence.GenOutput(writer)

	// ToDo:
	//TRM::Tube trm;
	//	trm.synthesizeToFile(trmParamStream, outputFile);
}

//SynthSequenceToBuf synthesizes speech from data contained in the event list
func (ctrl *Control) SynthSequenceToBuf(trmParaFile string, buf []float64) {
	f, err := os.Create(trmParaFile)
	if err != nil {
		log.Printf("Error trying to open %v\n", trmParaFile)
		return
	}
	writer := bufio.NewWriter(f)
	ctrl.InitUtterance(writer)
	ctrl.Sequence.GenOutput(writer)

	// ToDo:
	//TRM::Tube trm;
	//	trm.synthesizeToBuffer(trmParamStream, buf);
}

func (ctrl *Control) InitUtterance(w *bufio.Writer) {
	rc := ctrl.TrmConfig
	mc := ctrl.ModelConfig
	if rc.OutputRate != 22050.0 && rc.OutputRate != 44100.0 {
		rc.OutputRate = 44100.0
	}
	if rc.VtlOffset+rc.VocalTractLength < 15.9 {
		rc.OutputRate = 44100.0
	}

	ctrl.Sequence.PitchMean = ctrl.ModelConfig.PitchOffset + rc.RefGlottalPitch
	ctrl.Sequence.GlobalTempo = mc.Tempo
	ctrl.SetIntonation(mc.Intonation)
	ctrl.Sequence.Drift.SetUp(mc.DriftDeviation, mc.ControlRate, mc.DriftLowCutoff)
	ctrl.Sequence.SetRadiusCoefs(rc.RadiusCoefs)

	//f, err := os.Create("trmParams.txt")
	//if err != nil {
	//	log.Printf("Error trying to create %v\n", "trmParams.txt")
	//	return
	//}
	//w := bufio.NewWriter(f)
	w.WriteString(fmt.Sprintf("%f", rc.OutputRate) + "\n")
	w.WriteString(fmt.Sprintf("%f", mc.ControlRate) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.Volume) + "\n")
	w.WriteString(fmt.Sprintf("%d", rc.Channels) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.Balance) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.Waveform) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.GlottalPulseTp) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.GlottalPulseTnMin) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.GlottalPulseTnMax) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.Breathiness) + "\n")
	tvtl := rc.VtlOffset + rc.VocalTractLength
	w.WriteString(fmt.Sprintf("%f", tvtl) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.Temperature) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.LossFactor) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.ApertureRadius) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.MouthCoef) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.NoseCoef) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.NoseRadii[1]) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.NoseRadii[2]) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.NoseRadii[3]) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.NoseRadii[4]) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.NoseRadii[5]) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.ThroatCutoff) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.ThroatVolume) + "\n")
	w.WriteString(fmt.Sprintf("%d", rc.NoiseModulation) + "\n")
	w.WriteString(fmt.Sprintf("%f", rc.MixOffset) + "\n")
	w.Flush()
}

// Chunks are separated by /c.
// There is always one /c at the begin and another at the end of the string.
func (ctrl *Control) CalcChunks(text string) int {
	tmp := 0
	var idx int
	// C++ version relied on string terminator '\0'
	for idx = 0; idx < len(text)-1; {
		c := text[idx]
		d := text[idx+1]
		if c == '/' && d == 'c' {
			tmp++
			idx += 2
		} else {
			idx++
		}
	}
	tmp--
	if tmp < 0 {
		tmp = 0
	}
	return tmp
}

// NextChunk returns the position of the next /c (the position of the /).
func (ctrl *Control) NextChunk(text string) int {
	idx := 0
	length := len(text)
	for idx < length {
		if (text[idx] == '/') && (text[idx+1] == 'c') {
			return idx
		} else {
			idx++
		}
	}
	return 0
}

// ValidPosture
func (ctrl *Control) ValidPosture(token string) bool {
	i, err := strconv.Atoi(string(token[0]))
	if err != nil {
		return false
	}

	if i >= 0 && i <= 9 {
		return true
	} else {
		return ctrl.Model.PostureTry(token) != nil
	}
}

// SetIntonation
func (ctrl *Control) SetIntonation(intonation int64) {
	ctrl.Sequence.MicroFlag = false
	ctrl.Sequence.MacroFlag = false
	ctrl.Sequence.SmoothInton = false // Macro and not smooth is not working.
	ctrl.Sequence.DriftFlag = false
	ctrl.Sequence.TgUseRandom = false

	if bitflag.Has(intonation, int(IntonationMicro)) {
		ctrl.Sequence.MicroFlag = true
	}

	if bitflag.Has(intonation, int(IntonationMacro)) {
		ctrl.Sequence.MacroFlag = true
		ctrl.Sequence.SmoothInton = true // Macro and not smooth is not working.
	}

	// Macro and not smooth is not working.
	// if bitflag.Has(intonation, int(IntonationSmooth)) {
	// 	ctrl.Sequence.SetSmoothIntonation(1)
	// }

	if bitflag.Has(intonation, int(IntonationDrift)) {
		ctrl.Sequence.DriftFlag = true
	}

	if bitflag.Has(intonation, int(IntonationRandom)) {
		ctrl.Sequence.TgUseRandom = true
	}
}

type PhoneticParser interface {
	ParseString(s string) int
}

func (ctrl *Control) SynthPhoneticStringToFile(psp PhoneticParser, pString, trmParamFile, outputFile string) {
	f, err := os.Create(trmParamFile)
	if err != nil {
		log.Printf("Error trying to open %v\n", trmParamFile)
		return
	}
	writer := bufio.NewWriter(f)
	ctrl.PhoneticParse(psp, pString, writer)

	//trm := trm.Tube
	//trm.synthesizeToFile(writer, outputFile);
}

func (ctrl *Control) SynthPhoneticStringToBuf(psp PhoneticParser, pString, trmParamFile string, buf []float64) {
	f, err := os.Create(trmParamFile)
	if err != nil {
		log.Printf("Error trying to open %v\n", trmParamFile)
		return
	}
	writer := bufio.NewWriter(f)
	ctrl.PhoneticParse(psp, pString, writer)

	//trm := trm.Tube
	//trm.synthesizeToBuf(writer, buf);
}

func (ctrl *Control) PhoneticParse(psp PhoneticParser, pString string, writer *bufio.Writer) {
	chunks := ctrl.CalcChunks(pString)
	ctrl.InitUtterance(writer)
	idx := 0
	for chunks > 0 {
		log.Println("Speaking ", pString[idx])
		ctrl.SynthPhoneticStringChunk(psp, pString, writer)
		idx += ctrl.NextChunk(string(pString[idx+2])) + 2
		chunks--
	}
	writer.Reset(writer)
}

func (ctrl *Control) SynthPhoneticStringChunk(psp PhoneticParser, pschunk string, writer *bufio.Writer) {
	psp.ParseString(pschunk)
	ctrl.Sequence.GenerateEventList()
	ctrl.Sequence.ApplyIntonation()
	ctrl.Sequence.ApplyIntonationSmooth()
	ctrl.Sequence.GenOutput(writer)
}
