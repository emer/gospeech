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
 *  but WITHOUT ANY WARRANTY without even the implied warranty of         *
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
	"encoding/json"
	"fmt"
	"io/ioutil"
)

type TrmConfig struct {

	// output sample rate (22.05, 44.1)
	OutputRate float64 `desc:"output sample rate (22.05, 44.1)"`

	// master volume (0 - 60 dB)
	Volume float64 `desc:"master volume (0 - 60 dB)"`

	// # of sound output channels (1, 2)
	Channels int `desc:"# of sound output channels (1, 2)"`

	// stereo balance (-1 to +1)
	Balance float64 `desc:"stereo balance (-1 to +1)"`

	// GS waveform type (0=PULSE, 1=SINE)
	Waveform float64 `desc:"GS waveform type (0=PULSE, 1=SINE)"`

	// tube length offset
	VtlOffset float64 `desc:"tube length offset"`

	// tube temperature (25 - 40 C)
	Temperature float64 `desc:"tube temperature (25 - 40 C)"`

	// junction loss factor in (0 - 5 %)
	LossFactor float64 `desc:"junction loss factor in (0 - 5 %)"`

	// mouth aperture coefficient
	MouthCoef float64 `desc:"mouth aperture coefficient"`

	// nose aperture coefficient
	NoseCoef float64 `desc:"nose aperture coefficient"`

	// throat lp cutoff (50 - nyquist Hz)
	ThroatCutoff float64 `desc:"throat lp cutoff (50 - nyquist Hz)"`

	// throat volume (0 - 48 dB)
	ThroatVolume float64 `desc:"throat volume (0 - 48 dB)"`

	// pulse mod. of noise (0=OFF, 1=ON)
	NoiseModulation int `desc:"pulse mod. of noise (0=OFF, 1=ON)"`

	// noise crossmix offset (30 - 60 dB)
	MixOffset float64 `desc:"noise crossmix offset (30 - 60 dB)"`

	// % glottal pulse rise time
	GlottalPulseTp float64 `desc:"% glottal pulse rise time"`

	// % glottal pulse fall time minimum
	GlottalPulseTnMin float64 `desc:"% glottal pulse fall time minimum"`

	// % glottal pulse fall time maximum
	GlottalPulseTnMax float64 `desc:"% glottal pulse fall time maximum"`

	// % glottal source breathiness
	Breathiness      float64 `desc:"% glottal source breathiness"`
	VocalTractLength float64
	RefGlottalPitch  float64

	// aperture scl. radius (3.05 - 12 cm)
	ApertureRadius float64 `desc:"aperture scl. radius (3.05 - 12 cm)"`

	// fixed nose radii (0 - 3 cm)
	NoseRadii []float64 `desc:"fixed nose radii (0 - 3 cm)"`
	// ToDo: also shouldn't be hardcoded
	RadiusCoefs          []float64
	GlobalRadiusCoef     float64
	GlobalNoseRadiusCoef float64
}

// NewTrmConfig creates and returns a TrmConfig struct after making slices for NoseRadii and RadiusCoefs
func NewTrmConfig() *TrmConfig {
	tc := TrmConfig{}
	tc.NoseRadii = make([]float64, 6)   // ToDo: don't hard code
	tc.RadiusCoefs = make([]float64, 8) // ToDo: don't hard code
	return &tc
}

// Load will be passed data/en/trm_control_model.config or equivalent file
func (trm *TrmConfig) Load(pathTrm, pathVoice string) {
	fmt.Println("trm config load")
	trm.OpenJSON(pathTrm)
	trm.OpenJSON(pathVoice)
}

// OpenJSON opens model config from a JSON-formatted file (i.e. model params)
func (trm *TrmConfig) OpenJSON(fn string) error {
	b, err := ioutil.ReadFile(string(fn))
	if err != nil {
		return err
	}
	rval := json.Unmarshal(b, trm)
	return rval
}
