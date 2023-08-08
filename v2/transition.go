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
// This is a port to golang of the C++ Gnuspeech port by Marcelo Y. Matuda{

package v2

import (
	"github.com/goki/ki/kit"
)

// TransitionType
type TransitionType int

const (
	// TransInvalid
	TransInvalid TransitionType = iota

	// skip the value 1
	_

	// TransDiPhone
	TransDiPhone

	// TransTriPhone
	TransTriPhone

	// TransTetraPhone
	TransTetraPhone

	TransTypeN
)

//go:generate stringer -type=TransitionType

var Kit_TransitionType = kit.Enums.AddEnum(TransTypeN, kit.NotBitFlag, nil)

type PointOrSlope struct {
	//XMLName xml.Name `xml:"point-or-slopes"`
}

func (pos *PointOrSlope) IsSlopeRatio() bool {
	return false
}

// Point -- If timeExpression is not empty, time = timeExpression, otherwise time = freeTime.
type Point struct {
	PointOrSlope
	Type      TransitionType `xml:"type,attr"`
	Value     float64        `xml:"value,attr"`
	IsPhantom bool           `xml:"is-phantom,attr"`

	// milliseconds
	FreeTime float64   `xml:"free-time,attr" desc:"milliseconds"`
	TimeExpr *Equation `xml:"time-expression,attr"`
}

// NewPoint
func NewPoint() *Point {
	pt := Point{}
	pt.Type = TransInvalid
	pt.Value = 0.0
	pt.IsPhantom = false
	pt.FreeTime = 0.0
	pt.TimeExpr = nil
	return &pt
}

func (pt *Point) IsSlopeRatio() bool {
	return false
}

type Slope struct {
	PointOrSlope
	Slope       float64 `xml:"slope,attr"`
	DisplayTime float64 `xml:"display-time,attr"`
}

func NewSlope() *Slope {
	slp := Slope{}
	slp.Slope = 0.0
	slp.DisplayTime = 0.0
	return &slp
}

func (pos *Slope) IsSlopeRatio() bool {
	return false
}

type SlopeRatio struct {
	PointOrSlope
	Points []*Point `xml:"points>point"`
	Slopes []*Slope `xml:"slopes>slope"`
}

func NewSlopeRatio() *SlopeRatio {
	sr := SlopeRatio{}
	return &sr
}
func (sr *SlopeRatio) IsSlopeRatio() bool {
	return true
}

func (sr *SlopeRatio) NSlopeUnits() float64 {
	temp := 0.0
	for _, s := range sr.Slopes {
		temp += s.Slope
	}
	return temp
}

// Transition
type Transition struct {
	Name      string         `xml:"name,attr"`
	Type      TransitionType `xml:"type,attr"`
	PtSlpList []interface{}  `xml:"point-or-slopes>slope-ratio>points>point"`
	Comment   string         `xml:"comment"`
}

// PointTime
func PointTime(pt Point, model *Model) float64 {
	if pt.TimeExpr == nil {
		return pt.FreeTime
	} else {
		return pt.TimeExpr.EvalFormula(&model.FormulaVals)
	}
}

// PointData
func PointData(pt Point, model *Model) (time, value float64) {
	if pt.TimeExpr == nil {
		time = pt.FreeTime
	} else {
		time = model.EvalEquationFormula(pt.TimeExpr)
	}
	value = pt.Value
	return time, value
}

// PointDataMinMax
func PointDataMinMax(pt Point, model *Model, baseline, delta, min, max float64) (time, value float64) {
	if pt.TimeExpr == nil {
		time = pt.FreeTime
	} else {
		if &pt.TimeExpr.Formula != nil {
			time = model.EvalEquationFormula(pt.TimeExpr)
		}
	}

	value = baseline + (pt.Value/100.0)*delta
	if value < min {
		value = min
	} else if value > max {
		value = max
	}
	return time, value
}

type TransGrp struct {
	Name        string        `xml:"name,attr"`
	Transitions []*Transition `xml:"transition"`
}

type TransGrps struct {
	Grps []*TransGrp `xml:"transitions>transition-group"`
}

type TransGrpsSp struct {
	Grps []*TransGrp `xml:"special-transitions>transition-group"`
}
