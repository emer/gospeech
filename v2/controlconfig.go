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
	"log"

	"github.com/goki/ki/bitflag"
	"github.com/goki/ki/kit"
)

// IntonationFlags
type IntonationFlags int64

const (
	// 0x01
	IntonationMicro IntonationFlags = iota

	// 0x02
	IntonationMacro

	// 0x04
	IntonationSmooth

	// 0x08
	IntonationDrift

	// 0x10
	IntonationRandom

	IntonationFlagsN
)

//go:generate stringer -type=Intonation

var Kit_Intonation = kit.Enums.AddEnum(IntonationFlagsN, kit.BitFlag, nil)

type ModelConfig struct {
	Name  string
	Desc  string
	Voice string

	// 1.0-1000.0 input tables/second (Hz)
	ControlRate    float64 `desc:"1.0-1000.0 input tables/second (Hz)"`
	Tempo          float64
	PitchOffset    float64
	DriftDeviation float64
	DriftLowCutoff float64

	// Holds IntonationFlags
	Intonation int64 `desc:"Holds IntonationFlags"`

	// One of 5 types of intonation
	MicroIntonation int `desc:"One of 5 types of intonation"`

	// One of 5 types of intonation
	MacroIntonation int `desc:"One of 5 types of intonation"`

	// One of 5 types of intonation
	SmoothIntonation int `desc:"One of 5 types of intonation"`

	// One of 5 types of intonation
	DriftIntonation int `desc:"One of 5 types of intonation"`

	// One of 5 types of intonation
	RandomIntonation int `desc:"One of 5 types of intonation"`

	// Intonation parameters.
	NotionalPitch float64
	PretonicRange float64
	PretonicLift  float64
	TonicRange    float64
	TonicMovement float64

	Dictionary1 string
	Dictionary2 string
	Dictionary3 string
}

func (mc *ModelConfig) Defaults() {
	mc.ControlRate = 0.0
	mc.Tempo = 0.0
	mc.PitchOffset = 0.0
	mc.DriftDeviation = 0.0
	mc.DriftLowCutoff = 0.0
	mc.Intonation = 0
	mc.NotionalPitch = 0.0
	mc.PretonicRange = 0.0
	mc.PretonicLift = 0.0
	mc.TonicRange = 0.0
	mc.TonicMovement = 0.0
}

// Load will be passed data/en/trm_control_model.config or equivalent file
func (mc *ModelConfig) Load(path string) error {
	fmt.Println("model config load")
	err := mc.OpenJSON(path)
	if err != nil {
		log.Println("Load error: " + err.Error())
	}

	if mc.MicroIntonation == 1 {
		bitflag.Set(&mc.Intonation, int(IntonationMicro))
	}
	if mc.MacroIntonation == 1 {
		bitflag.Set(&mc.Intonation, int(IntonationMacro))
	}
	//if mc.SmoothIntonation == 1 {
	//	bitflag.Set(&mc.Intonation, int(IntonationSmooth))
	//}
	if mc.DriftIntonation == 1 {
		bitflag.Set(&mc.Intonation, int(IntonationDrift))
	}
	if mc.RandomIntonation == 1 {
		bitflag.Set(&mc.Intonation, int(IntonationRandom))
	}
	return nil
}

// OpenJSON opens model config from a JSON-formatted file (i.e. model params)
func (mc *ModelConfig) OpenJSON(fn string) error {
	b, err := ioutil.ReadFile(string(fn))
	if err != nil {
		return err
	}
	rval := json.Unmarshal(b, mc)
	return rval
}
