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

package trm

import (
	"bufio"
	"fmt"
	"github.com/emer/auditory/sound"
	"github.com/emer/etable/etable"
	"github.com/emer/etable/etensor"
	"github.com/go-audio/audio"
	"math"
)

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
	vtc.WaveForm = Pulse
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
	TractLength      float64    `desc:"length of vocal tract - shortest for baby voice, longest for male voice"`
	GlotPulseFallMin float64    `desc:"glottal pulse is the rate at which the vocal folds of the glottis open and close - not sure about FallMin/FallMax"`
	GlotPulseFallMax float64    `desc:"glottal pulse is the rate at which the vocal folds of the glottis open and close - not sure about FallMin/FallMax"`
	GlotPitchRef     float64    `desc:"the pitch of the voice is usually set by the frequency of glottal pulses during vowels or voiced consonants"`
	Breath           float64    `desc:"how much air escapes on each glottal pulse, more for female voices"`
	GlotPulseRise    float64    `desc:"glottal pulse is the rate at which the vocal folds of the glottis open and close - not sure about rise"`
	ApertureRadius   float64    `desc:""`
	NoseRadii        [6]float64 `desc:"fixed nose radii (0 - 3 cm)"`
	NoseRadiusCoef   float64    `desc:"global nose radius coefficient"`
	RadiusCoef       float64    `desc:""`
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
	GlotPitch float64    `min:"-10" max:"0" desc:"ranges from -10 for phoneme k to 0 for most, with some being -2 or -1 -- called microInt in gnuspeech data files"`
	GlotVol   float64    `min:"0" max:"60" desc:"glottal volume (DB?) typically 60 when present and 0 when not, and sometimes 54, 43.5, 42, "`
	AspVol    float64    `min:"0" max:"10" desc:"aspiration volume -- typically 0 when not present and 10 when present"`
	FricVol   float64    `min:"0" max:"24" desc:"fricative volume -- typically 0 or .25 .4, .5, .8 but 24 for ph"`
	FricPos   float64    `min:"1" max:"7" desc:"ficative position -- varies continuously between 1-7"`
	FricCf    float64    `min:"864" max:"5500" desc:"fricative center frequency ranges between 864 to 5500 with values around 1770, 2000, 2500, 4500 being common"`
	FricBw    float64    `min:"500" max:"4500" desc:"fricative bw seems like a frequency -- common intermediate values are 600, 900, 2000, 2600"`
	Radii     [7]float64 `desc:"Radii 2-8 radius of pharynx vocal tract segment as determined by tongue etc -- typically around 1, ranging .5 - 1.7"`
	Velum     float64    `min:".1" max:"1.5" desc:"velum opening -- 1.5 when fully open, .1 when closed, and .25, .5 intermediates used"`
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

// DefaultMaxDeltas updates the default max delta values in this object (for DeltaMax field in Tube)
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
//              Tube

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

type Tube struct {
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

	RateConverter         RateConverter
	MouthRadiationFilter  RadiationFilter
	MouthReflectionFilter ReflectionFilter
	NasalRadiationFilter  RadiationFilter
	NasalReflectionFilter ReflectionFilter
	Throat                Throat
	GlottalSource         WavetableGlottalSource
	BandpassFilter        BandpassFilter
	NoiseFilter           NoiseFilter
	NoiseSource           NoiseSource
}

// Init gets us going - this is the first function to call
func (tube *Tube) Init() {
	tube.Defaults()
	tube.Voice.Defaults()
	tube.Voice.SetAgeGender(Male)
	tube.Voice.Breath = 1.5 // ToDo: how is it getting set in C++ version and why isn't the male value!!
	tube.Params.Defaults()
	tube.InitSynth()
	tube.CurData.Defaults()
	tube.CurCtrl.SetFromParams(&tube.CurData)
	// do we need the next 2 set here?
	tube.PrvCtrl.SetFromParams(&tube.CurCtrl) // no deltas if reset
	tube.CurData.SetFromParams(&tube.CurCtrl)
}

func (tube *Tube) Defaults() {
	tube.Volume = 60.0
	tube.Balance = 0.0
	tube.Duration = 25.0
	tube.CtrlRate = 0.0
	tube.DeltaMax.DefaultMaxDeltas()
	tube.Reset()
	tube.SynthOutput = make([]float64, 0)
	tube.Wave = make([]float64, 0)
}

func (tube *Tube) ControlFromTable(col etensor.Tensor, row int, normalized bool) {
	params := col.SubSpace([]int{row}).(*etensor.Float64)
	tube.CurCtrl.SetFromValues(params.Values)
}

// Reset reset all vocal tract values
func (tube *Tube) Reset() {
	tube.CtrlPeriod = 0
	tube.TubeLength = 0.0
	for i := 0; i < OroPharynxSectCnt; i++ {
		for j := 0; j < 2; j++ {
			for k := 0; k < 2; k++ {
				tube.Oropharynx[i][j][k] = 0.0
			}
		}
	}
	for i := 0; i < OroPharynxCoefCnt; i++ {
		tube.OropharynxCoefs[i] = 0.0
	}

	for i := 0; i < NasalSectCnt; i++ {
		for j := 0; j < 2; j++ {
			for k := 0; k < 2; k++ {
				tube.Nasal[i][j][k] = 0.0
			}
		}
	}
	for i := 0; i < NasalCoefCnt; i++ {
		tube.NasalCoefs[i] = 0.0
	}
	for i := 0; i < ThreeWayCnt; i++ {
		tube.Alpha[i] = 0.0
	}
	for i := 0; i < FricationInjCoefCnt; i++ {
		tube.FricationTap[i] = 0.0
	}
	tube.CurPtr = 1
	tube.PrvPtr = 0
	tube.DampingFactor = 0.0
	tube.CrossmixFactor = 0.0
	tube.BreathFactor = 0.0
	tube.PrvGlotAmplitude = -1.0
	tube.SynthOutput = nil
	tube.Wave = nil
	tube.RateConverter.Reset()
	tube.MouthRadiationFilter.Reset()
	tube.MouthReflectionFilter.Reset()
	tube.NasalRadiationFilter.Reset()
	tube.NasalReflectionFilter.Reset()
	tube.Throat.Reset()
	tube.GlottalSource.Reset()
	tube.BandpassFilter.Reset()
	tube.NoiseFilter.Reset()
	tube.NoiseSource.Reset()
}

// SpeedOfSound returns the speed of sound according to the value of the temperature (in Celsius degrees)
func SpeedOfSound(temp float64) float64 {
	return 331.4 + (0.6 * temp)
}

//InitializeSynthesizer initializes all variables so that the synthesis can be run
func (tube *Tube) InitializeSynthesizer() {
	var nyquist float64

	// calculate the sample rate, based on nominal tube length and speed of sound
	if tube.Voice.TractLength > 0.0 {
		c := SpeedOfSound(tube.Params.Temp)
		tube.CtrlPeriod = int(math.Round(float64(c*OroPharynxSectCnt*100.0) / float64(tube.Voice.TractLength*tube.CtrlRate)))
		tube.SampleRate = int(tube.CtrlRate * float64(tube.CtrlPeriod))
		tube.TubeLength = float64(c*OroPharynxSectCnt*100.0) / float64(tube.SampleRate)
		nyquist = float64(tube.SampleRate) / 2.0
	} else {
		nyquist = 1.0
		fmt.Println("Illegal tube length")
	}
	tube.BreathFactor = tube.Voice.Breath / 100.0
	tube.CrossmixFactor = 1.0 / Amplitude(tube.Params.MixOff)
	tube.DampingFactor = (1.0 - (tube.Params.Loss / 100.0))

	// initialize the wave table
	gs := WavetableGlottalSource{}
	tube.GlottalSource = gs
	tube.GlottalSource.Init(GlottalSourcePulse, float64(tube.SampleRate), tube.Voice.GlotPulseRise, tube.Voice.GlotPulseFallMin, tube.Voice.GlotPulseFallMax)
	tube.GlottalSource.Reset()

	mouthApertureCoef := (nyquist - tube.Params.MouthCoef) / nyquist
	tube.MouthRadiationFilter.Init(mouthApertureCoef)
	tube.MouthRadiationFilter.Reset()
	tube.MouthReflectionFilter.Init(mouthApertureCoef)
	tube.MouthReflectionFilter.Reset()

	nasalApertureCoef := (nyquist - tube.Params.NoseCoef) / nyquist
	tube.NasalRadiationFilter.Init(nasalApertureCoef)
	tube.NasalRadiationFilter.Reset()
	tube.NasalReflectionFilter.Init(nasalApertureCoef)
	tube.NasalReflectionFilter.Reset()

	tube.InitNasal()
	tube.Throat.Init(float64(tube.SampleRate), tube.Params.ThroatCutoff, Amplitude(tube.Params.ThroatVol))
	tube.Throat.Reset()

	tube.RateConverter.Init(tube.SampleRate, OutputRate, &tube.SynthOutput)
	tube.RateConverter.Reset()
	for i := 0; i < len(tube.SynthOutput); i++ {
		tube.SynthOutput[i] = 0
	}

	tube.BandpassFilter.Reset()
	tube.NoiseFilter.Reset()
	tube.NoiseSource.Reset()
}

func (tube *Tube) InitSynth() {
	tube.SampleRate = 44100
	tube.InitSndBuf(0, 1, tube.SampleRate, 16)
	tube.Reset()
	tube.CtrlRate = 1.0 / (tube.Duration / 1000.0)
	tube.InitializeSynthesizer()
	tube.PrvCtrl.SetFromParams(&tube.CurCtrl)
	tube.CurData.SetFromParams(&tube.CurCtrl)
}

// InitBuffer
func (tube *Tube) InitSndBuf(frames int, channels, rate, bitDepth int) {
	//frames := (tube.Duration / 1000.0) * float64(tube.SampleRate)
	format := &audio.Format{
		NumChannels: channels,
		SampleRate:  rate,
	}
	if tube.Buf.Buf == nil {
		tube.Buf.Buf = &audio.IntBuffer{Data: make([]int, 0), Format: format, SourceBitDepth: 16}
		//tube.Buf.Buf = &audio.IntBuffer{Data: make([]int, 0), Format: format, SourceBitDepth: 16}
	}
}

// InitBuffer
func (tube *Tube) ResizeSndBuf(frames int) {
	data := make([]int, int(frames))
	tube.Buf.Buf.Data = data
}

// SynthReset
func (tube *Tube) SynthReset(initBuffer bool) {
	tube.InitSynth()
	if initBuffer {
		tube.InitSndBuf(0, 1, tube.SampleRate, 16)
	}
}

// SynthSignal
func (tube *Tube) Synth() {
	// convert parameters here
	f0 := Frequency(tube.CurData.GlotPitch)
	ax := Amplitude(tube.CurData.GlotVol)
	ah1 := Amplitude(tube.CurData.AspVol)
	tube.TubeCoefficients()
	tube.SetFricationTaps()
	tube.BandpassFilter.Update(float64(tube.SampleRate), float64(tube.CurData.FricBw), float64(tube.CurData.FricCf))

	// do synthesis here
	// create low-pass filtered noise
	lpNoise := tube.NoiseFilter.Filter(tube.NoiseSource.GetSample())

	// update the shape of the glottal pulse, if necessary
	if tube.Params.WaveForm == Pulse {
		if ax != tube.PrvGlotAmplitude {
			tube.GlottalSource.Update(ax)
		}
	}

	//  create glottal pulse (or sine tone)
	pulse := tube.GlottalSource.GetSample(f0)
	pulsedNoise := lpNoise * pulse

	// create noisy glottal pulse
	pulse = ax * ((pulse * (1.0 - tube.BreathFactor)) + (pulsedNoise * tube.BreathFactor))

	var signal float64
	// cross-mix pure noise with pulsed noise
	if tube.Params.NoiseMod {
		crossmix := ax * tube.CrossmixFactor
		if crossmix >= 1.0 {
			crossmix = 1.0
		}
		signal = (pulsedNoise * crossmix) + (lpNoise * (1.0 - crossmix))
	} else {
		signal = lpNoise
	}

	signal = tube.Update(((pulse + (ah1 * signal)) * VtScale), float64(tube.BandpassFilter.Filter(float64(signal))))
	signal += tube.Throat.Process(pulse * VtScale)

	// output sample here
	tube.RateConverter.DataFill(signal)
	tube.PrvGlotAmplitude = ax
}

// InitNasalCavity
func (tube *Tube) InitNasal() {
	var radA2, radB2 float64

	// calculate coefficients for internal fixed sections of nasal cavity
	for i, j := NasalS2, NasalC2; i < NasalS6; i, j = i+1, j+1 {
		radA2 = tube.Voice.NoseRadii[i]
		radA2 *= radA2
		radB2 = tube.Voice.NoseRadii[i+1]
		radB2 *= radB2
		tube.NasalCoefs[j] = (radA2 - radB2) / (radA2 + radB2)
	}

	// calculate the fixed coefficient for the nose aperture
	radA2 = tube.Voice.NoseRadii[NasalS6] // zero based
	radA2 *= radA2
	radB2 = tube.Voice.ApertureRadius * tube.Voice.ApertureRadius
	tube.NasalCoefs[NasalC6] = (radA2 - radB2) / (radA2 + radB2)
}

// TubeCoefficients
func (tube *Tube) TubeCoefficients() {
	var radA2, radB2 float64
	// calculate coefficients for the oropharynx
	for i := 0; i < OroPharynxRegCnt-1; i++ {
		radA2 = tube.CurData.RadiusVal(i)
		radA2 *= radA2
		radB2 = tube.CurData.RadiusVal(i + 1)
		radB2 *= radB2
		tube.OropharynxCoefs[i] = (radA2 - radB2) / (radA2 + radB2)
	}

	// calculate the coefficient for the mouth aperture
	radA2 = tube.CurData.RadiusVal(OroPharynxReg8)
	radA2 *= radA2
	radB2 = tube.Voice.ApertureRadius * tube.Voice.ApertureRadius
	tube.OropharynxCoefs[OroPharynxC8] = (radA2 - radB2) / (radA2 + radB2)

	// calculate alpha coefficients for 3-way junction
	// note:  since junction is in middle of region 4, r0_2 = r1_2
	r0_2 := tube.CurData.RadiusVal(OroPharynxReg4)
	r0_2 *= r0_2
	r1_2 := r0_2
	r2_2 := tube.CurData.Velum * tube.CurData.Velum
	sum := 2.0 / (r0_2 + r1_2 + r2_2)
	tube.Alpha[ThreeWayLeft] = sum * r0_2
	tube.Alpha[ThreeWayRight] = sum * r1_2
	tube.Alpha[ThreeWayUpper] = sum * r2_2

	// and 1st nasal passage coefficient
	radA2 = tube.CurData.Velum * tube.CurData.Velum
	radB2 = tube.Voice.NoseRadii[NasalS2]
	radB2 *= radB2
	tube.NasalCoefs[NasalC1] = (radA2 - radB2) / (radA2 + radB2)
}

// SetFricationTaps Sets frication taps according to the current position and amplitude of frication
func (tube *Tube) SetFricationTaps() {
	fricationAmplitude := Amplitude(tube.CurData.FricVol)

	integerPart := int(tube.CurData.FricPos)
	complement := tube.CurData.FricPos - float64(integerPart)
	remainder := 1.0 - complement

	for i := FricationInjC1; i < FricationInjCoefCnt; i++ {
		if i == int(integerPart) {
			tube.FricationTap[i] = remainder * fricationAmplitude
			if (i + 1) < FricationInjCoefCnt {
				i += 1
				tube.FricationTap[i] = complement * fricationAmplitude
			}
		} else {
			tube.FricationTap[i] = 0.0
		}
	}
}

// Update updates the pressure wave throughout the vocal tract, and returns
// the summed output of the oral and nasal cavities.  Also injects frication appropriately
func (tube *Tube) Update(input, frication float64) (output float64) {
	tube.CurPtr += 1
	if tube.CurPtr > 1 {
		tube.CurPtr = 0
	}

	tube.PrvPtr += 1
	if tube.PrvPtr > 1 {
		tube.PrvPtr = 0
	}
	// input to top of tube
	tube.Oropharynx[OroPharynxS1][Top][tube.CurPtr] =
		(tube.Oropharynx[OroPharynxS1][Bottom][tube.PrvPtr] * tube.DampingFactor) + input

	// calculate the scattering junctions for s1-s2
	delta := tube.OropharynxCoefs[OroPharynxC1] *
		(tube.Oropharynx[OroPharynxS1][Top][tube.PrvPtr] - tube.Oropharynx[OroPharynxS2][Bottom][tube.PrvPtr])
	tube.Oropharynx[OroPharynxS2][Top][tube.CurPtr] =
		(tube.Oropharynx[OroPharynxS1][Top][tube.PrvPtr] + delta) * tube.DampingFactor
	tube.Oropharynx[OroPharynxS1][Bottom][tube.CurPtr] =
		(tube.Oropharynx[OroPharynxS2][Bottom][tube.PrvPtr] + delta) * tube.DampingFactor

	// calculate the scattering junctions for s2-s3 and s3-s4
	for i, j, k := OroPharynxS2, OroPharynxC2, FricationInjC1; i < OroPharynxS4; i, j, k = i+1, j+1, k+1 {
		delta = tube.OropharynxCoefs[j] *
			(tube.Oropharynx[i][Top][tube.PrvPtr] - tube.Oropharynx[i+1][Bottom][tube.PrvPtr])
		tube.Oropharynx[i+1][Top][tube.CurPtr] =
			((tube.Oropharynx[i][Top][tube.PrvPtr] + delta) * tube.DampingFactor) +
				(tube.FricationTap[k] * frication)
		tube.Oropharynx[i][Bottom][tube.CurPtr] =
			(tube.Oropharynx[i+1][Bottom][tube.PrvPtr] + delta) * tube.DampingFactor
	}

	// update 3-way junction between the middle of R4 and nasal cavity
	junctionPressure := (tube.Alpha[ThreeWayLeft] * tube.Oropharynx[OroPharynxS4][Top][tube.PrvPtr]) +
		(tube.Alpha[ThreeWayRight] * tube.Oropharynx[OroPharynxS5][Bottom][tube.PrvPtr]) +
		(tube.Alpha[ThreeWayUpper] * tube.Nasal[Velum][Bottom][tube.PrvPtr])
	tube.Oropharynx[OroPharynxS4][Bottom][tube.CurPtr] =
		(junctionPressure - tube.Oropharynx[OroPharynxS4][Top][tube.PrvPtr]) * tube.DampingFactor
	tube.Oropharynx[OroPharynxS5][Top][tube.CurPtr] =
		((junctionPressure - tube.Oropharynx[OroPharynxS5][Bottom][tube.PrvPtr]) * tube.DampingFactor) + (tube.FricationTap[FricationInjC3] * frication)
	tube.Nasal[Velum][Top][tube.CurPtr] =
		(junctionPressure - tube.Nasal[Velum][Bottom][tube.PrvPtr]) * tube.DampingFactor

	// calculate junction between R4 and R5 (S5-S6)
	delta = tube.OropharynxCoefs[OroPharynxC4] *
		(tube.Oropharynx[OroPharynxS5][Top][tube.PrvPtr] - tube.Oropharynx[OroPharynxS6][Bottom][tube.PrvPtr])
	tube.Oropharynx[OroPharynxS6][Top][tube.CurPtr] =
		((tube.Oropharynx[OroPharynxS5][Top][tube.PrvPtr] + delta) * tube.DampingFactor) +
			(tube.FricationTap[FricationInjC4] * frication)
	tube.Oropharynx[OroPharynxS5][Bottom][tube.CurPtr] =
		(tube.Oropharynx[OroPharynxS6][Bottom][tube.PrvPtr] + delta) * tube.DampingFactor

	// Calculate junction inside R5 (S6-S7) (pure delay with damping)
	tube.Oropharynx[OroPharynxS7][Top][tube.CurPtr] =
		(tube.Oropharynx[OroPharynxS6][Top][tube.PrvPtr] * tube.DampingFactor) +
			(tube.FricationTap[FricationInjC5] * frication)
	tube.Oropharynx[OroPharynxS6][Bottom][tube.CurPtr] =
		tube.Oropharynx[OroPharynxS7][Bottom][tube.PrvPtr] * tube.DampingFactor

	// calculate last 3 internal junctions (S7-S8, S8-S9, S9-S10)
	for i, j, k := OroPharynxS7, OroPharynxC5, FricationInjC6; i < OroPharynxS10; i, j, k = i+1, j+1, k+1 {
		delta = tube.OropharynxCoefs[j] *
			(tube.Oropharynx[i][Top][tube.PrvPtr] - tube.Oropharynx[i+1][Bottom][tube.PrvPtr])
		tube.Oropharynx[i+1][Top][tube.CurPtr] =
			((tube.Oropharynx[i][Top][tube.PrvPtr] + delta) * tube.DampingFactor) +
				(tube.FricationTap[k] * frication)
		tube.Oropharynx[i][Bottom][tube.CurPtr] =
			(tube.Oropharynx[i+1][Bottom][tube.PrvPtr] + delta) * tube.DampingFactor
	}

	// reflected signal at mouth goes through a lowpass filter
	tube.Oropharynx[OroPharynxS10][Bottom][tube.CurPtr] = tube.DampingFactor *
		tube.MouthReflectionFilter.Filter(tube.OropharynxCoefs[OroPharynxC8]*
			tube.Oropharynx[OroPharynxS10][Top][tube.PrvPtr])

	// output from mouth goes through a highpass filter
	output = tube.MouthRadiationFilter.Filter((1.0 + tube.OropharynxCoefs[OroPharynxC8]) *
		tube.Oropharynx[OroPharynxS10][Top][tube.PrvPtr])

	//  update nasal cavity
	for i, j := Velum, NasalC1; i < NasalC6; i, j = i+1, j+1 {
		delta = tube.NasalCoefs[j] *
			(tube.Nasal[i][Top][tube.PrvPtr] - tube.Nasal[i+1][Bottom][tube.PrvPtr])
		tube.Nasal[i+1][Top][tube.CurPtr] =
			(tube.Nasal[i][Top][tube.PrvPtr] + delta) * tube.DampingFactor
		tube.Nasal[i][Bottom][tube.CurPtr] =
			(tube.Nasal[i+1][Bottom][tube.PrvPtr] + delta) * tube.DampingFactor
	}

	// reflected signal at nose goes through a lowpass filter
	tube.Nasal[NasalS6][Bottom][tube.CurPtr] = tube.DampingFactor *
		tube.NasalReflectionFilter.Filter(tube.NasalCoefs[NasalC6]*tube.Nasal[NasalC6][Top][tube.PrvPtr])

	// output from nose goes through a highpass filter
	output += tube.NasalRadiationFilter.Filter((1.0 + tube.NasalCoefs[NasalC6]) *
		tube.Nasal[NasalS6][Top][tube.PrvPtr])

	// return summed output from mouth and nose
	return output
}

// MonoScale
func (tube *Tube) MonoScale() float64 {
	return (OutputScale / (tube.RateConverter.MaxSampleVal()) * Amplitude(tube.Volume))
}

// StereoScale
func (tube *Tube) StereoScale(leftScale,
	rightScale *float64) {
	*leftScale = (-((tube.Balance / 2.0) - 0.5))
	*rightScale = (-((tube.Balance / 2.0) + 0.5))

	scale := leftScale
	if tube.Balance > 0.0 {
		scale = rightScale
	}
	newMax := (tube.RateConverter.MaxSampleVal() * (*scale))
	*scale = (OutputScale / (newMax * Amplitude(tube.Volume)))
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

// SynthToFile
func (tube *Tube) SynthesizeToFile(w *bufio.Writer, outFile string) {
	if len(outFile) == 0 {
		//reset();
	}
	//tube.ParseInputStream(w)
	tube.InitializeSynthesizer()

	//tube.SynthesizeForInputSequence()
	//writeOutputToFile(outputFile)
}

// SynthSignal
func (tube *Tube) SynthSignal() {
	// convert parameters here
	f0 := Frequency(tube.CurData.GlotPitch)
	ax := Amplitude(tube.CurData.GlotVol)
	ah1 := Amplitude(tube.CurData.AspVol)
	tube.TubeCoefficients()
	tube.SetFricationTaps()
	tube.BandpassFilter.Update(float64(tube.SampleRate), float64(tube.CurData.FricBw), float64(tube.CurData.FricCf))

	// do synthesis here
	// create low-pass filtered noise
	lpNoise := tube.NoiseFilter.Filter(tube.NoiseSource.GetSample())

	// update the shape of the glottal pulse, if necessary
	if tube.Params.WaveForm == Pulse {
		if ax != tube.PrvGlotAmplitude {
			tube.GlottalSource.Update(ax)
		}
	}

	//  create glottal pulse (or sine tone)
	pulse := tube.GlottalSource.GetSample(f0)
	pulsedNoise := lpNoise * pulse

	// create noisy glottal pulse
	pulse = ax * ((pulse * (1.0 - tube.BreathFactor)) + (pulsedNoise * tube.BreathFactor))

	var signal float64
	// cross-mix pure noise with pulsed noise
	if tube.Params.NoiseMod {
		crossmix := ax * tube.CrossmixFactor
		if crossmix >= 1.0 {
			crossmix = 1.0
		}
		signal = (pulsedNoise * crossmix) + (lpNoise * (1.0 - crossmix))
	} else {
		signal = lpNoise
	}

	signal = tube.Update(((pulse + (ah1 * signal)) * VtScale), float64(tube.BandpassFilter.Filter(float64(signal))))
	signal += tube.Throat.Process(pulse * VtScale)

	// output sample here
	tube.RateConverter.DataFill(signal)
	tube.PrvGlotAmplitude = ax
}
