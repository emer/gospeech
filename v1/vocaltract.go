// Copyright (c) 2019, The Emergent Authors. All rights reserved.
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

/******************************************************************************
*
*     Program:       tube
*
*     Description:   Software (non-real-time) implementation of the Tube
*                    Resonance Model for speech production.
*
*     Author:        Leonard Manzara
*
*     Date:          July 5th, 1994
*
******************************************************************************/

// 2019-02
// This is a port to golang of the C++ Gnuspeech port by Marcelo Y. Matuda

package v1

import (
	"errors"
	"fmt"
	"math"
	"strings"

	"github.com/emer/auditory/sound"
	"github.com/emer/etable/etable"
	"github.com/emer/etable/etensor"
	"github.com/emer/gospeech/trm"
	"github.com/go-audio/audio"
	"github.com/goki/gi/gi"
)

/*  COMPILE SO THAT INTERPOLATION NOT DONE FOR SOME CONTROL RATE PARAMETERS  */
//#define MATCH_DSP                 1

const GsTrmTubeMinRadius = 0.001
const OutputSize = 1024
const GlottalSourcePulse = 0
const GlottalSourceSine = 1
const PitchBase = 220.0
const PitchOffset = 3
const VolMax = 60
const VtScale = 0.125
const OutputScale = 0.95
const Top = 0
const Bottom = 1

/////////////////////////////////////////////////////
//              TractParams

type TractParams struct {
	Temp         float64
	Loss         float64
	MouthCoef    float64
	NoseCoef     float64
	ThroatCutoff float64
	ThroatVol    float64
	VtlOff       float64
	MixOff       float64
	WaveForm     int32
	NoiseMod     bool
}

// Defaults sets the default values for the vocal tract
func (vtc *TractParams) Defaults() {
	vtc.Temp = 32.0
	vtc.Loss = 0.8
	vtc.MouthCoef = 5000.0
	vtc.NoseCoef = 5000.0
	vtc.ThroatCutoff = 1500.0
	vtc.ThroatVol = 6.0
	vtc.VtlOff = 0.0
	vtc.WaveForm = trm.Pulse
	vtc.NoiseMod = true
	vtc.MixOff = 48.0
}

/////////////////////////////////////////////////////
//              VoiceParams

type AgeGender int32

const (
	Male = iota
	Female
	ChildLg
	ChildSm
	Baby
)

//go:generate stringer -type=Voices

// VoiceParams are the parameters that control the quality of the voice
type VoiceParams struct {

	// length of vocal tract - shortest for baby voice, longest for male voice
	TractLength float64 `desc:"length of vocal tract - shortest for baby voice, longest for male voice"`

	// glottal pulse is the rate at which the vocal folds of the glottis open and close - not sure about FallMin/FallMax
	GlotPulseFallMin float64 `desc:"glottal pulse is the rate at which the vocal folds of the glottis open and close - not sure about FallMin/FallMax"`

	// glottal pulse is the rate at which the vocal folds of the glottis open and close - not sure about FallMin/FallMax
	GlotPulseFallMax float64 `desc:"glottal pulse is the rate at which the vocal folds of the glottis open and close - not sure about FallMin/FallMax"`

	// the pitch of the voice is usually set by the frequency of glottal pulses during vowels or voiced consonants
	GlotPitchRef float64 `desc:"the pitch of the voice is usually set by the frequency of glottal pulses during vowels or voiced consonants"`

	// how much air escapes on each glottal pulse, more for female voices
	Breath float64 `desc:"how much air escapes on each glottal pulse, more for female voices"`

	// glottal pulse is the rate at which the vocal folds of the glottis open and close - not sure about rise
	GlotPulseRise  float64 `desc:"glottal pulse is the rate at which the vocal folds of the glottis open and close - not sure about rise"`
	ApertureRadius float64 `desc:""`

	// fixed nose radii (0 - 3 cm)
	NoseRadii [6]float64 `desc:"fixed nose radii (0 - 3 cm)"`

	// global nose radius coefficient
	NoseRadiusCoef float64 `desc:"global nose radius coefficient"`
	RadiusCoef     float64 `desc:""`
}

// DefaultParams are the defaults, some of which don't change
func (vp *VoiceParams) Defaults() {
	vp.GlotPulseRise = 40.0
	vp.ApertureRadius = 3.05
	vp.NoseRadii[0] = 1.35
	vp.NoseRadii[1] = 1.96
	vp.NoseRadii[2] = 1.91
	vp.NoseRadii[3] = 1.3
	vp.NoseRadii[4] = 0.73
	vp.NoseRadii[5] = 0.8 // called Radius_1 in c++ code
	//vp.Radius = 0.8
	vp.NoseRadiusCoef = 1.0
	vp.RadiusCoef = 1.0
}

func (vp *VoiceParams) Male() {
	vp.TractLength = 17.5
	vp.GlotPulseFallMin = 24.0
	vp.GlotPulseFallMax = 24.0
	vp.GlotPitchRef = -12.0
	vp.Breath = 0.5
}

func (vp *VoiceParams) Female() {
	vp.TractLength = 15.0
	vp.GlotPulseFallMin = 32.0
	vp.GlotPulseFallMax = 32.0
	vp.GlotPitchRef = 0.0
	vp.Breath = 1.5
}

func (vp *VoiceParams) ChildLg() {
	vp.TractLength = 12.5
	vp.GlotPulseFallMin = 24.0
	vp.GlotPulseFallMax = 24.0
	vp.GlotPitchRef = 2.5
	vp.Breath = 1.5
}

func (vp *VoiceParams) ChildSm() {
	vp.TractLength = 10.0
	vp.GlotPulseFallMin = 24.0
	vp.GlotPulseFallMax = 24.0
	vp.GlotPitchRef = 5.0
	vp.Breath = 1.5
}

func (vp *VoiceParams) Baby() {
	vp.TractLength = 7.5
	vp.GlotPulseFallMin = 24.0
	vp.GlotPulseFallMax = 24.0
	vp.GlotPitchRef = 7.5
	vp.Breath = 1.5
}

// SetAgeGender is used to set the voicing parameters to one of several predefined voice param sets
func (vp *VoiceParams) SetAgeGender(voice AgeGender) {
	switch voice {
	case Male:
		vp.Male()
	case Female:
		vp.Female()
	case ChildLg:
		vp.ChildLg()
	case ChildSm:
		vp.ChildSm()
	case Baby:
		vp.Baby()
	}
}

/////////////////////////////////////////////////////
//              TractCtrl

// ToDo: desc for all Radii
type TractCtrl struct {

	// [min: -10] [max: 0] ranges from -10 for phoneme k to 0 for most, with some being -2 or -1 -- called microInt in gnuspeech data files
	GlotPitch float64 `min:"-10" max:"0" desc:"ranges from -10 for phoneme k to 0 for most, with some being -2 or -1 -- called microInt in gnuspeech data files"`

	// [min: 0] [max: 60] glottal volume (DB?) typically 60 when present and 0 when not, and sometimes 54, 43.5, 42,
	GlotVol float64 `min:"0" max:"60" desc:"glottal volume (DB?) typically 60 when present and 0 when not, and sometimes 54, 43.5, 42, "`

	// [min: 0] [max: 10] aspiration volume -- typically 0 when not present and 10 when present
	AspVol float64 `min:"0" max:"10" desc:"aspiration volume -- typically 0 when not present and 10 when present"`

	// [min: 0] [max: 24] fricative volume -- typically 0 or .25 .4, .5, .8 but 24 for ph
	FricVol float64 `min:"0" max:"24" desc:"fricative volume -- typically 0 or .25 .4, .5, .8 but 24 for ph"`

	// [min: 1] [max: 7] ficative position -- varies continuously between 1-7
	FricPos float64 `min:"1" max:"7" desc:"ficative position -- varies continuously between 1-7"`

	// [min: 864] [max: 5500] fricative center frequency ranges between 864 to 5500 with values around 1770, 2000, 2500, 4500 being common
	FricCf float64 `min:"864" max:"5500" desc:"fricative center frequency ranges between 864 to 5500 with values around 1770, 2000, 2500, 4500 being common"`

	// [min: 500] [max: 4500] fricative bw seems like a frequency -- common intermediate values are 600, 900, 2000, 2600
	FricBw float64 `min:"500" max:"4500" desc:"fricative bw seems like a frequency -- common intermediate values are 600, 900, 2000, 2600"`

	// Radii 2-8 radius of pharynx vocal tract segment as determined by tongue etc -- typically around 1, ranging .5 - 1.7
	Radii [7]float64 `desc:"Radii 2-8 radius of pharynx vocal tract segment as determined by tongue etc -- typically around 1, ranging .5 - 1.7"`

	// [min: .1] [max: 1.5] velum opening -- 1.5 when fully open, .1 when closed, and .25, .5 intermediates used
	Velum float64 `min:".1" max:"1.5" desc:"velum opening -- 1.5 when fully open, .1 when closed, and .25, .5 intermediates used"`
}

func (vtc *TractCtrl) Defaults() {
	vtc.GlotPitch = 0.0
	vtc.GlotVol = 0.0
	vtc.AspVol = 0.0
	vtc.FricVol = 0.0
	vtc.FricPos = 4.0
	vtc.FricCf = 2500.0
	vtc.FricBw = 2000.0
	for i, _ := range vtc.Radii {
		vtc.Radii[i] = 1.0
	}
	vtc.Velum = 0.1
}

// ComputeDeltas computes values in this set of params as deltas from (cur - prv) * ctrl_freq
func (vtc *TractCtrl) ComputeDeltas(cur, prv *TractCtrl, cf float64) {
	vtc.GlotPitch = (cur.GlotPitch - prv.GlotPitch) * cf
	vtc.GlotVol = (cur.GlotVol - prv.GlotVol) * cf
	vtc.AspVol = (cur.AspVol - prv.AspVol) * cf
	vtc.FricVol = (cur.FricVol - prv.FricVol) * cf
	vtc.FricPos = (cur.FricPos - prv.FricPos) * cf
	vtc.FricCf = (cur.FricCf - prv.FricCf) * cf
	vtc.FricBw = (cur.FricBw - prv.FricBw) * cf
	for i, _ := range vtc.Radii {
		vtc.Radii[i] = (cur.Radii[i] - prv.Radii[i]) * cf
	}
	vtc.Velum = (cur.Velum - prv.Velum) * cf
}

// UpdateFromDeltas updates values in this set of params from deltas
func (vtc *TractCtrl) UpdateFromDeltas(deltas *TractCtrl) {
	vtc.GlotPitch += deltas.GlotPitch
	vtc.GlotVol += deltas.GlotVol
	vtc.AspVol += deltas.AspVol
	vtc.FricVol += deltas.FricVol
	vtc.FricPos += deltas.FricPos
	vtc.FricCf += deltas.FricCf
	vtc.FricBw += deltas.FricBw
	for i, _ := range vtc.Radii {
		vtc.Radii[i] += deltas.Radii[i]
	}
	vtc.Velum += deltas.Velum
}

// DefaultMaxDeltas updates the default max delta values in this object (for DeltaMax field in VocalTract)
func (vtc *TractCtrl) DefaultMaxDeltas() {
	cf := float64(1.0 / 501.0) // default control frequency
	// default to entire range ok for now.. fix when glitches encountered.. (comment from c++ code)
	vtc.GlotPitch = 10 * cf
	vtc.GlotVol = 60.0 * cf
	vtc.AspVol = 10.0 * cf
	vtc.FricVol = 24.0 * cf
	vtc.FricPos = 7.0 * cf
	vtc.FricCf = 3000.0 * cf
	vtc.FricBw = 4000.0 * cf
	for i, _ := range vtc.Radii {
		vtc.Radii[i] = 3.0 * cf
	}
	vtc.Velum = 1.5 * cf
}

// SetFromParams fast copy of parameters from other control params
func (vtc *TractCtrl) SetFromParams(src *TractCtrl) {
	vtc.GlotPitch = src.GlotPitch
	vtc.GlotVol = src.GlotVol
	vtc.AspVol = src.AspVol
	vtc.FricVol = src.FricVol
	vtc.FricPos = src.FricPos
	vtc.FricCf = src.FricCf
	vtc.FricBw = src.FricBw
	for i, _ := range vtc.Radii {
		vtc.Radii[i] = src.Radii[i]
	}
	vtc.Velum = src.Velum
}

// SetFromValues - order must be preserved!
func (vtc *TractCtrl) SetFromValues(values []float64) {
	vtc.GlotPitch = values[0]
	vtc.GlotVol = values[1]
	vtc.AspVol = values[2]
	vtc.FricVol = values[3]
	vtc.FricPos = values[4]
	vtc.FricCf = values[5]
	vtc.FricBw = values[6]
	for i, _ := range vtc.Radii {
		vtc.Radii[i] = values[i+7]
	}
	vtc.Velum = values[14]
}

func (vtc *TractCtrl) RadiusVal(idx int) float64 {
	if idx <= 0 {
		return 0.8
	}
	v := vtc.Radii[idx-1]
	return v
	// C++ code was return (&radius_2)[idx-1]; }
}

/////////////////////////////////////////////////////
//              VocalTract

// OroPharynxRegions are different regions of the vocal tract
type OroPharynxRegions int32

const (
	OroPharynxReg1 = iota // S1
	OroPharynxReg2        // S2
	OroPharynxReg3        // S3
	OroPharynxReg4        // S4 & S5
	OroPharynxReg5        // S6 & S7
	OroPharynxReg6        // S8
	OroPharynxReg7        // S9
	OroPharynxReg8        // S10
	OroPharynxRegCnt
)

//go:generate stringer -type=OroPharynxRegions

// OroPharynxCoefs are the oropharynx scattering junction coefficients (between each region)
type OroPharynxCoefs int32

const (
	OroPharynxC1 = iota // R1-R2 (S1-S2)
	OroPharynxC2        // R2-R3 (S2-S3)
	OroPharynxC3        // R3-R4 (S3-S4)
	OroPharynxC4        // R4-R5 (S5-S6)
	OroPharynxC5        // R5-R6 (S7-S8)
	OroPharynxC6        // R6-R7 (S8-S9)
	OroPharynxC7        // R7-R8 (S9-S10)
	OroPharynxC8        // R8-Air (S10-Air)
	OroPharynxCoefCnt
)

//go:generate stringer -type=OroPharynxCoefs

// OroPharynxCoefs are the oropharynx scattering junction coefficients (between each region)
type OroPharynxSects int32

const (
	OroPharynxS1  = iota // OroPharynxReg1
	OroPharynxS2         // OroPharynxReg2
	OroPharynxS3         // OroPharynxReg3
	OroPharynxS4         // OroPharynxReg4
	OroPharynxS5         // OroPharynxReg4
	OroPharynxS6         // OroPharynxReg5
	OroPharynxS7         // OroPharynxReg5
	OroPharynxS8         // OroPharynxReg6
	OroPharynxS9         // OroPharynxReg7
	OroPharynxS10        // OroPharynxReg8
	OroPharynxSectCnt
)

//go:generate stringer -type=OroPharynxSects

// NasalSections are different sections of the nasal tract
type NasalSections int32

const (
	NasalS1 = iota
	NasalS2
	NasalS3
	NasalS4
	NasalS5
	NasalS6
	NasalSectCnt
	Velum = NasalS1
)

//go:generate stringer -type=NasalSections

// NasalCoefs
type NasalCoefs int32

const (
	NasalC1      = NasalS1 // N1-N2
	NasalC2      = NasalS2 // N2-N3
	NasalC3      = NasalS3 // N3-N4
	NasalC4      = NasalS4 // N4-N5
	NasalC5      = NasalS5 // N5-N6
	NasalC6      = NasalS6 // N6-Air
	NasalCoefCnt = NasalSectCnt
)

//go:generate stringer -type=NasalCoefs

// ThreeWayJunction for the three-way junction alpha coefficients
type ThreeWayJunction int32

const (
	ThreeWayLeft = iota
	ThreeWayRight
	ThreeWayUpper
	ThreeWayCnt
)

//go:generate stringer -type=ThreeWayJunction

// FricationInjCoefs are the oropharynx scattering junction coefficients (between each region)
type FricationInjCoefs int32

const (
	FricationInjC1 = iota // S3
	FricationInjC2        // S4
	FricationInjC3        // S5
	FricationInjC4        // S6
	FricationInjC5        // S7
	FricationInjC6        // S8
	FricationInjC7        // S9
	FricationInjC8        // S10
	FricationInjCoefCnt
)

//go:generate stringer -type=FricationInjCoefs

type VocalTract struct {
	Buf        sound.Wave   `desc:""`
	Volume     float64      `desc:""`
	Balance    float64      `desc:""`
	Duration   float64      `desc:""` // duration of synthesized sound
	Params     TractParams  `desc:""`
	Voice      VoiceParams  `desc:""`
	CurCtrl    TractCtrl    `desc:""`
	PrvCtrl    TractCtrl    `desc:""`
	DeltaCtrl  TractCtrl    `desc:""`
	DeltaMax   TractCtrl    `desc:""`
	PhoneTable etable.Table `desc:""`
	Dictionary etable.Table `desc:""`

	// derived values
	CtrlRate   float64 `desc:""` // 1.0-1000.0 input tables/second (Hz)
	CtrlPeriod int     `desc:""`
	SampleRate int     `desc:""`
	TubeLength float64 `desc:""` // actual length in cm

	CurData TractCtrl `desc:""` // current control data

	// tube and tube coefficients
	Oropharynx      [OroPharynxSectCnt][2][2]float64
	OropharynxCoefs [OroPharynxCoefCnt]float64
	Nasal           [NasalSectCnt][2][2]float64
	NasalCoefs      [NasalCoefCnt]float64
	Alpha           [ThreeWayCnt]float64
	CurPtr          int
	PrvPtr          int

	// memory for frication taps
	FricationTap [FricationInjCoefCnt]float64

	DampingFactor    float64 // calculated
	CrossmixFactor   float64 //  calculated
	BreathFactor     float64
	PrvGlotAmplitude float64

	SynthOutput []float64
	Wave        []float64

	RateConverter         trm.RateConverter
	MouthRadiationFilter  trm.RadiationFilter
	MouthReflectionFilter trm.ReflectionFilter
	NasalRadiationFilter  trm.RadiationFilter
	NasalReflectionFilter trm.ReflectionFilter
	Throat                trm.Throat
	GlottalSource         trm.WavetableGlottalSource
	BandpassFilter        trm.BandpassFilter
	NoiseFilter           trm.NoiseFilter
	NoiseSource           trm.NoiseSource
}

// Init gets us going - this is the first function to call
func (vt *VocalTract) Init() {
	vt.Defaults()
	vt.Voice.Defaults()
	vt.Voice.SetAgeGender(Male)
	vt.Voice.Breath = 1.5 // ToDo: how is it getting set in C++ version and why isn't the male value!!
	vt.Params.Defaults()
	vt.InitSynth()
	vt.CurData.Defaults()
	vt.CurCtrl.SetFromParams(&vt.CurData)
	// do we need the next 2 set here?
	vt.PrvCtrl.SetFromParams(&vt.CurCtrl) // no deltas if reset
	vt.CurData.SetFromParams(&vt.CurCtrl)
}

func (vt *VocalTract) Defaults() {
	vt.Volume = 60.0
	vt.Balance = 0.0
	vt.Duration = 25.0
	vt.CtrlRate = 0.0
	vt.DeltaMax.DefaultMaxDeltas()
	vt.Reset()
	vt.SynthOutput = make([]float64, 0)
	vt.Wave = make([]float64, 0)
}

func (vt *VocalTract) ControlFromTable(col etensor.Tensor, row int, normalized bool) {
	params := col.SubSpace([]int{row}).(*etensor.Float64)
	vt.CurCtrl.SetFromValues(params.Values)
}

// LoadEnglishPhones loads the file of English phones
func (vt *VocalTract) LoadEnglishPhones() {
	fn := gi.FileName("VocalTractEnglishPhones64.dat") // table saved with columns as double precision
	err := vt.PhoneTable.OpenCSV(fn, '\t')
	if err != nil {
		fmt.Printf("File not found or error opengin file: %s (%s)", fn, err)
		return
	}
}

// LoadDictionary loads the English dictionary of words composed of phones and transitions
func (vt *VocalTract) LoadDictionary() {
	fn := gi.FileName("VocalTractEnglishDict2.dat")
	err := vt.Dictionary.OpenCSV(fn, '\t')
	if err != nil {
		fmt.Printf("File not found or error open file: %s (%s)", fn, err)
		return
	}
}

// SynthPhone is an *internal* call - call synthphones even for a single phone
func (vt *VocalTract) synthPhone(phon string, stress, doubleStress, syllable, reset bool) bool {
	if vt.PhoneTable.Rows == 0 {
		vt.LoadEnglishPhones()
	}
	if stress {
		phon = phon + "'"
	}
	pcol := vt.PhoneTable.ColByName("phone")
	idx := -1
	for i := 0; i < pcol.Len(); i++ {
		if pcol.StringVal1D(i) == phon {
			idx = i
			break
		}
	}
	if idx < 0 {
		return false
	}

	dc := vt.PhoneTable.ColByName("duration")
	dv := dc.FloatVal1D(idx)
	tc := vt.PhoneTable.ColByName("transition")
	tv := tc.FloatVal1D(idx)
	tt := (dv + tv) * 1.5

	nReps := math.Ceil(tt / float64(vt.Duration))
	nReps = math.Max(nReps, 1.0)

	vt.ControlFromTable(vt.PhoneTable.ColByName("phone_data"), idx, false)
	// todo: syllable, double_stress, qsss other params??
	// fmt.Println("saying:", phon, "dur:", String(tot_time), "n_reps:", String(n_reps),
	//              "start pos:", String(outputData_.size()));
	if reset {
		vt.SynthReset(true)
	}
	for i := 0; i < int(nReps); i++ {
		vt.Synth(false)
	}
	return true
}

// SynthPhones
func (vt *VocalTract) SynthPhones(phones string, resetFirst, play bool) bool {
	var phone string
	stress := false
	doubleStress := false
	syllable := false
	first := true

	for _, r := range phones {
		c := string(r)
		if c == "'" {
			stress = true
			continue
		}
		if c == "~" {
			doubleStress = true
			continue
		}
		if c == "%" {
			vt.synthPhone(phone, stress, doubleStress, syllable, resetFirst && first)
			phone = ""
			first = false
			break // done
		}
		if c == "." { // syllable
			syllable = true
			vt.synthPhone(phone, stress, doubleStress, syllable, resetFirst && first)
			stress = false
			doubleStress = false
			syllable = false
			phone = ""
			first = false
			continue
		}
		if c == "_" { // reg separator
			vt.synthPhone(phone, stress, doubleStress, syllable, resetFirst && first)
			stress = false
			doubleStress = false
			syllable = false
			phone = ""
			first = false
			continue
		}
		phone += c
	}
	if len(phone) > 0 {
		vt.synthPhone(phone, stress, doubleStress, syllable, resetFirst && first)
	}

	if play {
		PlaySound()
	}
	return true
}

// SynthWord
func (vt *VocalTract) SynthWord(word string, resetFirst bool, play bool) bool {
	if vt.Dictionary.Rows == 0 {
		vt.LoadDictionary()
	}
	col := vt.Dictionary.ColByName("word")
	if col == nil {
		fmt.Printf("Column name 'word' not found")
		return false
	}

	var idx = -1
	for i := 0; i < col.Len(); i++ {
		if col.StringVal1D(i) == word {
			idx = i
		}
	}
	if idx == -1 {
		return false
	}
	col = vt.Dictionary.ColByName("phones")
	if col == nil {
		fmt.Printf("Column name 'phones' not found")
		return false
	}
	phones := col.StringVal1D(idx)
	return vt.SynthPhones(phones, resetFirst, play)
}

// SynthWords
func (vt *VocalTract) SynthWords(ws string, resetFirst bool, play bool) (rval bool, err error) {
	words := strings.Split(ws, " ")
	for i := 0; i < len(words); i++ {
		rval = vt.SynthWord(words[i], (resetFirst && (i == 0)), false)
		if !rval {
			msg := "The word \"" + words[i] + "\" is not in the dictionary"
			err = errors.New(msg)
			return rval, err
		}
		if i < len(words)-1 {
			vt.synthPhone("#", false, false, false, false)
		}
	}
	if play {
		PlaySound()
	}
	return rval, nil
}

// Reset reset all vocal tract values
func (vt *VocalTract) Reset() {
	vt.CtrlPeriod = 0
	vt.TubeLength = 0.0
	for i := 0; i < OroPharynxSectCnt; i++ {
		for j := 0; j < 2; j++ {
			for k := 0; k < 2; k++ {
				vt.Oropharynx[i][j][k] = 0.0
			}
		}
	}
	for i := 0; i < OroPharynxCoefCnt; i++ {
		vt.OropharynxCoefs[i] = 0.0
	}

	for i := 0; i < NasalSectCnt; i++ {
		for j := 0; j < 2; j++ {
			for k := 0; k < 2; k++ {
				vt.Nasal[i][j][k] = 0.0
			}
		}
	}
	for i := 0; i < NasalCoefCnt; i++ {
		vt.NasalCoefs[i] = 0.0
	}
	for i := 0; i < ThreeWayCnt; i++ {
		vt.Alpha[i] = 0.0
	}
	for i := 0; i < FricationInjCoefCnt; i++ {
		vt.FricationTap[i] = 0.0
	}
	vt.CurPtr = 1
	vt.PrvPtr = 0
	vt.DampingFactor = 0.0
	vt.CrossmixFactor = 0.0
	vt.BreathFactor = 0.0
	vt.PrvGlotAmplitude = -1.0
	vt.SynthOutput = nil
	vt.Wave = nil
	vt.RateConverter.Reset()
	vt.MouthRadiationFilter.Reset()
	vt.MouthReflectionFilter.Reset()
	vt.NasalRadiationFilter.Reset()
	vt.NasalReflectionFilter.Reset()
	vt.Throat.Reset()
	vt.GlottalSource.Reset()
	vt.BandpassFilter.Reset()
	vt.NoiseFilter.Reset()
	vt.NoiseSource.Reset()
}

// SpeedOfSound returns the speed of sound according to the value of the temperature (in Celsius degrees)
func SpeedOfSound(temp float64) float64 {
	return 331.4 + (0.6 * temp)
}

// InitializeSynthesizer initializes all variables so that the synthesis can be run
func (vt *VocalTract) InitializeSynthesizer() {
	var nyquist float64

	// calculate the sample rate, based on nominal tube length and speed of sound
	if vt.Voice.TractLength > 0.0 {
		c := SpeedOfSound(vt.Params.Temp)
		vt.CtrlPeriod = int(math.Round(float64(c*OroPharynxSectCnt*100.0) / float64(vt.Voice.TractLength*vt.CtrlRate)))
		vt.SampleRate = int(vt.CtrlRate * float64(vt.CtrlPeriod))
		vt.TubeLength = float64(c*OroPharynxSectCnt*100.0) / float64(vt.SampleRate)
		nyquist = float64(vt.SampleRate) / 2.0
	} else {
		nyquist = 1.0
		fmt.Println("Illegal tube length")
	}
	vt.BreathFactor = vt.Voice.Breath / 100.0
	vt.CrossmixFactor = 1.0 / Amplitude(vt.Params.MixOff)
	vt.DampingFactor = (1.0 - (vt.Params.Loss / 100.0))

	// initialize the wave table
	gs := trm.WavetableGlottalSource{}
	vt.GlottalSource = gs
	vt.GlottalSource.Init(GlottalSourcePulse, float64(vt.SampleRate), vt.Voice.GlotPulseRise, vt.Voice.GlotPulseFallMin, vt.Voice.GlotPulseFallMax)
	vt.GlottalSource.Reset()

	mouthApertureCoef := (nyquist - vt.Params.MouthCoef) / nyquist
	vt.MouthRadiationFilter.Init(mouthApertureCoef)
	vt.MouthRadiationFilter.Reset()
	vt.MouthReflectionFilter.Init(mouthApertureCoef)
	vt.MouthReflectionFilter.Reset()

	nasalApertureCoef := (nyquist - vt.Params.NoseCoef) / nyquist
	vt.NasalRadiationFilter.Init(nasalApertureCoef)
	vt.NasalRadiationFilter.Reset()
	vt.NasalReflectionFilter.Init(nasalApertureCoef)
	vt.NasalReflectionFilter.Reset()

	vt.InitNasal()
	vt.Throat.Init(float64(vt.SampleRate), vt.Params.ThroatCutoff, Amplitude(vt.Params.ThroatVol))
	vt.Throat.Reset()

	vt.RateConverter.Init(vt.SampleRate, trm.OutputRate, &vt.SynthOutput)
	vt.RateConverter.Reset()
	for i := 0; i < len(vt.SynthOutput); i++ {
		vt.SynthOutput[i] = 0
	}

	vt.BandpassFilter.Reset()
	vt.NoiseFilter.Reset()
	vt.NoiseSource.Reset()
}

func (vt *VocalTract) InitSynth() {
	vt.SampleRate = 44100
	vt.InitSndBuf(0, 1, vt.SampleRate, 16)
	vt.Reset()
	vt.CtrlRate = 1.0 / (vt.Duration / 1000.0)
	vt.InitializeSynthesizer()
	vt.PrvCtrl.SetFromParams(&vt.CurCtrl)
	vt.CurData.SetFromParams(&vt.CurCtrl)
}

// InitBuffer
func (vt *VocalTract) InitSndBuf(frames int, channels, rate, bitDepth int) {
	//frames := (vt.Duration / 1000.0) * float64(vt.SampleRate)
	format := &audio.Format{
		NumChannels: channels,
		SampleRate:  rate,
	}
	if vt.Buf.Buf == nil {
		vt.Buf.Buf = &audio.IntBuffer{Data: make([]int, 0), Format: format, SourceBitDepth: 16}
		//vt.Buf.Buf = &audio.IntBuffer{Data: make([]int, 0), Format: format, SourceBitDepth: 16}
	}
}

// InitBuffer
func (vt *VocalTract) ResizeSndBuf(frames int) {
	data := make([]int, int(frames))
	vt.Buf.Buf.Data = data
}

// SynthReset
func (vt *VocalTract) SynthReset(initBuffer bool) {
	vt.InitSynth()
	if initBuffer {
		vt.InitSndBuf(0, 1, vt.SampleRate, 16)
	}
}

// Synth set params before making a call to synthesize the signal and then outputs the signal
func (vt *VocalTract) Synth(reset bool) {
	ctrlRate := 1.0 / (vt.Duration / 1000.0)
	if ctrlRate != vt.CtrlRate { // todo: || !IsValid()
		vt.InitSynth()
	} else if reset {
		vt.SynthReset(true)
	}

	controlFreq := 1.0 / float64(vt.CtrlPeriod)
	vt.DeltaCtrl.ComputeDeltas(&vt.CurCtrl, &vt.PrvCtrl, float64(controlFreq))

	for j := 0; j < vt.CtrlPeriod; j++ {
		vt.SynthSignal()
		vt.CurData.UpdateFromDeltas(&vt.DeltaCtrl)
	}
	vt.PrvCtrl.SetFromParams(&vt.CurData) // prev is where we actually got, not where we wanted to get..

	vt.ResizeSndBuf(len(vt.SynthOutput))
	scale := vt.MonoScale()
	vt.Wave = nil
	vt.Wave = make([]float64, len(vt.SynthOutput))
	for i := 0; i < len(vt.SynthOutput); i++ {
		vt.Wave[i] = vt.SynthOutput[i] * scale
		vt.Buf.Buf.Data[i] = int(vt.SynthOutput[i] * scale * 32767) // scale to normalize, (when writing wave file multiply by max signed int)
	}
}

// SynthSignal
func (vt *VocalTract) SynthSignal() {
	// convert parameters here
	f0 := Frequency(vt.CurData.GlotPitch)
	ax := Amplitude(vt.CurData.GlotVol)
	ah1 := Amplitude(vt.CurData.AspVol)
	vt.TubeCoefficients()
	vt.SetFricationTaps()
	vt.BandpassFilter.Update(float64(vt.SampleRate), float64(vt.CurData.FricBw), float64(vt.CurData.FricCf))

	// do synthesis here
	// create low-pass filtered noise
	lpNoise := vt.NoiseFilter.Filter(vt.NoiseSource.GetSample())

	// update the shape of the glottal pulse, if necessary
	if vt.Params.WaveForm == trm.Pulse {
		if ax != vt.PrvGlotAmplitude {
			vt.GlottalSource.Update(ax)
		}
	}

	//  create glottal pulse (or sine tone)
	pulse := vt.GlottalSource.GetSample(f0)
	pulsedNoise := lpNoise * pulse

	// create noisy glottal pulse
	pulse = ax * ((pulse * (1.0 - vt.BreathFactor)) + (pulsedNoise * vt.BreathFactor))

	var signal float64
	// cross-mix pure noise with pulsed noise
	if vt.Params.NoiseMod {
		crossmix := ax * vt.CrossmixFactor
		if crossmix >= 1.0 {
			crossmix = 1.0
		}
		signal = (pulsedNoise * crossmix) + (lpNoise * (1.0 - crossmix))
	} else {
		signal = lpNoise
	}

	signal = vt.Update(((pulse + (ah1 * signal)) * VtScale), float64(vt.BandpassFilter.Filter(float64(signal))))
	signal += vt.Throat.Process(pulse * VtScale)

	// output sample here
	vt.RateConverter.DataFill(signal)
	vt.PrvGlotAmplitude = ax
}

// InitNasalCavity
func (vt *VocalTract) InitNasal() {
	var radA2, radB2 float64

	// calculate coefficients for internal fixed sections of nasal cavity
	for i, j := NasalS2, NasalC2; i < NasalS6; i, j = i+1, j+1 {
		radA2 = vt.Voice.NoseRadii[i]
		radA2 *= radA2
		radB2 = vt.Voice.NoseRadii[i+1]
		radB2 *= radB2
		vt.NasalCoefs[j] = (radA2 - radB2) / (radA2 + radB2)
	}

	// calculate the fixed coefficient for the nose aperture
	radA2 = vt.Voice.NoseRadii[NasalS6] // zero based
	radA2 *= radA2
	radB2 = vt.Voice.ApertureRadius * vt.Voice.ApertureRadius
	vt.NasalCoefs[NasalC6] = (radA2 - radB2) / (radA2 + radB2)
}

// TubeCoefficients
func (vt *VocalTract) TubeCoefficients() {
	var radA2, radB2 float64
	// calculate coefficients for the oropharynx
	for i := 0; i < OroPharynxRegCnt-1; i++ {
		radA2 = vt.CurData.RadiusVal(i)
		radA2 *= radA2
		radB2 = vt.CurData.RadiusVal(i + 1)
		radB2 *= radB2
		vt.OropharynxCoefs[i] = (radA2 - radB2) / (radA2 + radB2)
	}

	// calculate the coefficient for the mouth aperture
	radA2 = vt.CurData.RadiusVal(OroPharynxReg8)
	radA2 *= radA2
	radB2 = vt.Voice.ApertureRadius * vt.Voice.ApertureRadius
	vt.OropharynxCoefs[OroPharynxC8] = (radA2 - radB2) / (radA2 + radB2)

	// calculate alpha coefficients for 3-way junction
	// note:  since junction is in middle of region 4, r0_2 = r1_2
	r0_2 := vt.CurData.RadiusVal(OroPharynxReg4)
	r0_2 *= r0_2
	r1_2 := r0_2
	r2_2 := vt.CurData.Velum * vt.CurData.Velum
	sum := 2.0 / (r0_2 + r1_2 + r2_2)
	vt.Alpha[ThreeWayLeft] = sum * r0_2
	vt.Alpha[ThreeWayRight] = sum * r1_2
	vt.Alpha[ThreeWayUpper] = sum * r2_2

	// and 1st nasal passage coefficient
	radA2 = vt.CurData.Velum * vt.CurData.Velum
	radB2 = vt.Voice.NoseRadii[NasalS2]
	radB2 *= radB2
	vt.NasalCoefs[NasalC1] = (radA2 - radB2) / (radA2 + radB2)
}

// SetFricationTaps Sets frication taps according to the current position and amplitude of frication
func (vt *VocalTract) SetFricationTaps() {
	fricationAmplitude := Amplitude(vt.CurData.FricVol)

	integerPart := int(vt.CurData.FricPos)
	complement := vt.CurData.FricPos - float64(integerPart)
	remainder := 1.0 - complement

	for i := FricationInjC1; i < FricationInjCoefCnt; i++ {
		if i == int(integerPart) {
			vt.FricationTap[i] = remainder * fricationAmplitude
			if (i + 1) < FricationInjCoefCnt {
				i += 1
				vt.FricationTap[i] = complement * fricationAmplitude
			}
		} else {
			vt.FricationTap[i] = 0.0
		}
	}
}

// Update updates the pressure wave throughout the vocal tract, and returns
// the summed output of the oral and nasal cavities.  Also injects frication appropriately
func (vt *VocalTract) Update(input, frication float64) (output float64) {
	vt.CurPtr += 1
	if vt.CurPtr > 1 {
		vt.CurPtr = 0
	}

	vt.PrvPtr += 1
	if vt.PrvPtr > 1 {
		vt.PrvPtr = 0
	}
	// input to top of tube
	vt.Oropharynx[OroPharynxS1][Top][vt.CurPtr] =
		(vt.Oropharynx[OroPharynxS1][Bottom][vt.PrvPtr] * vt.DampingFactor) + input

	// calculate the scattering junctions for s1-s2
	delta := vt.OropharynxCoefs[OroPharynxC1] *
		(vt.Oropharynx[OroPharynxS1][Top][vt.PrvPtr] - vt.Oropharynx[OroPharynxS2][Bottom][vt.PrvPtr])
	vt.Oropharynx[OroPharynxS2][Top][vt.CurPtr] =
		(vt.Oropharynx[OroPharynxS1][Top][vt.PrvPtr] + delta) * vt.DampingFactor
	vt.Oropharynx[OroPharynxS1][Bottom][vt.CurPtr] =
		(vt.Oropharynx[OroPharynxS2][Bottom][vt.PrvPtr] + delta) * vt.DampingFactor

	// calculate the scattering junctions for s2-s3 and s3-s4
	for i, j, k := OroPharynxS2, OroPharynxC2, FricationInjC1; i < OroPharynxS4; i, j, k = i+1, j+1, k+1 {
		delta = vt.OropharynxCoefs[j] *
			(vt.Oropharynx[i][Top][vt.PrvPtr] - vt.Oropharynx[i+1][Bottom][vt.PrvPtr])
		vt.Oropharynx[i+1][Top][vt.CurPtr] =
			((vt.Oropharynx[i][Top][vt.PrvPtr] + delta) * vt.DampingFactor) +
				(vt.FricationTap[k] * frication)
		vt.Oropharynx[i][Bottom][vt.CurPtr] =
			(vt.Oropharynx[i+1][Bottom][vt.PrvPtr] + delta) * vt.DampingFactor
	}

	// update 3-way junction between the middle of R4 and nasal cavity
	junctionPressure := (vt.Alpha[ThreeWayLeft] * vt.Oropharynx[OroPharynxS4][Top][vt.PrvPtr]) +
		(vt.Alpha[ThreeWayRight] * vt.Oropharynx[OroPharynxS5][Bottom][vt.PrvPtr]) +
		(vt.Alpha[ThreeWayUpper] * vt.Nasal[Velum][Bottom][vt.PrvPtr])
	vt.Oropharynx[OroPharynxS4][Bottom][vt.CurPtr] =
		(junctionPressure - vt.Oropharynx[OroPharynxS4][Top][vt.PrvPtr]) * vt.DampingFactor
	vt.Oropharynx[OroPharynxS5][Top][vt.CurPtr] =
		((junctionPressure - vt.Oropharynx[OroPharynxS5][Bottom][vt.PrvPtr]) * vt.DampingFactor) + (vt.FricationTap[FricationInjC3] * frication)
	vt.Nasal[Velum][Top][vt.CurPtr] =
		(junctionPressure - vt.Nasal[Velum][Bottom][vt.PrvPtr]) * vt.DampingFactor

	// calculate junction between R4 and R5 (S5-S6)
	delta = vt.OropharynxCoefs[OroPharynxC4] *
		(vt.Oropharynx[OroPharynxS5][Top][vt.PrvPtr] - vt.Oropharynx[OroPharynxS6][Bottom][vt.PrvPtr])
	vt.Oropharynx[OroPharynxS6][Top][vt.CurPtr] =
		((vt.Oropharynx[OroPharynxS5][Top][vt.PrvPtr] + delta) * vt.DampingFactor) +
			(vt.FricationTap[FricationInjC4] * frication)
	vt.Oropharynx[OroPharynxS5][Bottom][vt.CurPtr] =
		(vt.Oropharynx[OroPharynxS6][Bottom][vt.PrvPtr] + delta) * vt.DampingFactor

	// Calculate junction inside R5 (S6-S7) (pure delay with damping)
	vt.Oropharynx[OroPharynxS7][Top][vt.CurPtr] =
		(vt.Oropharynx[OroPharynxS6][Top][vt.PrvPtr] * vt.DampingFactor) +
			(vt.FricationTap[FricationInjC5] * frication)
	vt.Oropharynx[OroPharynxS6][Bottom][vt.CurPtr] =
		vt.Oropharynx[OroPharynxS7][Bottom][vt.PrvPtr] * vt.DampingFactor

	// calculate last 3 internal junctions (S7-S8, S8-S9, S9-S10)
	for i, j, k := OroPharynxS7, OroPharynxC5, FricationInjC6; i < OroPharynxS10; i, j, k = i+1, j+1, k+1 {
		delta = vt.OropharynxCoefs[j] *
			(vt.Oropharynx[i][Top][vt.PrvPtr] - vt.Oropharynx[i+1][Bottom][vt.PrvPtr])
		vt.Oropharynx[i+1][Top][vt.CurPtr] =
			((vt.Oropharynx[i][Top][vt.PrvPtr] + delta) * vt.DampingFactor) +
				(vt.FricationTap[k] * frication)
		vt.Oropharynx[i][Bottom][vt.CurPtr] =
			(vt.Oropharynx[i+1][Bottom][vt.PrvPtr] + delta) * vt.DampingFactor
	}

	// reflected signal at mouth goes through a lowpass filter
	vt.Oropharynx[OroPharynxS10][Bottom][vt.CurPtr] = vt.DampingFactor *
		vt.MouthReflectionFilter.Filter(vt.OropharynxCoefs[OroPharynxC8]*
			vt.Oropharynx[OroPharynxS10][Top][vt.PrvPtr])

	// output from mouth goes through a highpass filter
	output = vt.MouthRadiationFilter.Filter((1.0 + vt.OropharynxCoefs[OroPharynxC8]) *
		vt.Oropharynx[OroPharynxS10][Top][vt.PrvPtr])

	//  update nasal cavity
	for i, j := Velum, NasalC1; i < NasalC6; i, j = i+1, j+1 {
		delta = vt.NasalCoefs[j] *
			(vt.Nasal[i][Top][vt.PrvPtr] - vt.Nasal[i+1][Bottom][vt.PrvPtr])
		vt.Nasal[i+1][Top][vt.CurPtr] =
			(vt.Nasal[i][Top][vt.PrvPtr] + delta) * vt.DampingFactor
		vt.Nasal[i][Bottom][vt.CurPtr] =
			(vt.Nasal[i+1][Bottom][vt.PrvPtr] + delta) * vt.DampingFactor
	}

	// reflected signal at nose goes through a lowpass filter
	vt.Nasal[NasalS6][Bottom][vt.CurPtr] = vt.DampingFactor *
		vt.NasalReflectionFilter.Filter(vt.NasalCoefs[NasalC6]*vt.Nasal[NasalC6][Top][vt.PrvPtr])

	// output from nose goes through a highpass filter
	output += vt.NasalRadiationFilter.Filter((1.0 + vt.NasalCoefs[NasalC6]) *
		vt.Nasal[NasalS6][Top][vt.PrvPtr])

	// return summed output from mouth and nose
	return output
}

// MonoScale
func (vt *VocalTract) MonoScale() float64 {
	return (OutputScale / (vt.RateConverter.MaxSampleVal()) * Amplitude(vt.Volume))
}

// StereoScale
func (vt *VocalTract) StereoScale(leftScale,
	rightScale *float64) {
	*leftScale = (-((vt.Balance / 2.0) - 0.5))
	*rightScale = (-((vt.Balance / 2.0) + 0.5))

	scale := leftScale
	if vt.Balance > 0.0 {
		scale = rightScale
	}
	newMax := (vt.RateConverter.MaxSampleVal() * (*scale))
	*scale = (OutputScale / (newMax * Amplitude(vt.Volume)))
	*leftScale *= *scale
	*rightScale *= *scale
}

// Amplitude  converts dB value to amplitude value
func Amplitude(decibelLevel float64) float64 {
	decibelLevel -= VolMax

	if decibelLevel <= -VolMax {
		return 0
	}

	if decibelLevel >= 0.0 {
		return 1.0
	}

	return math.Pow(10.0, decibelLevel/20.0)
}

// Frequency converts a given pitch (0 = middle C) to the corresponding frequency
func Frequency(pitch float64) float64 {
	return PitchBase * math.Pow(2.0, (pitch+PitchOffset)/12.0)
}

func PlaySound() {

}
