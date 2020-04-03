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

import "github.com/goki/ki/kit"

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
}

func (pos *PointOrSlope) IsSlopeRatio() bool {
	return false
}

type Point struct {
	PointOrSlope
	TType     TransitionType
	Value     float64
	IsPhantom bool

	// If timeExpression is not empty, time = timeExpression, otherwise time = freeTime.
	timeExpr *Equation
	FreeTime float64 `desc:"milliseconds""`
}

func NewPoint() *Point {
	pt := Point{}
	pt.TType = TransInvalid
	pt.Value = 0.0
	pt.IsPhantom = false
	pt.FreeTime = 0.0
	pt.timeExpr = &Equation{}
	return &pt
}

func (pt *Point) IsSlopeRatio() bool {
	return false
}

type Slope struct {
	PointOrSlope
	Slope       float64
	DisplayTime float64
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
	Points []Point
	Slopes []Slope
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
	Name      string
	TType     TransitionType
	Special   bool
	PtSlpList []interface{}
}

// PointTime
func PointTime(pt Point, model *Model) float64 {
	if pt.timeExpr != nil {
		return pt.FreeTime
	} else {
		return pt.timeExpr.Eval(model.Formula)
	}
}

// PointData
func PointData(pt Point, model *Model) (time, value float64) {
	if pt.timeExpr != nil {
		time = pt.FreeTime
	} else {
		time = pt.timeExpr.Eval(model.Formula)
	}
	value = pt.Value
	return time, value
}

// PointDataMinMax
func PointDataMinMax(pt Point, model *Model, baseline, delta, min, max float64) (time, value float64) {
	if pt.timeExpr != nil {
		time = pt.FreeTime
	} else {
		time = pt.timeExpr.Eval(model.Formula)
	}

	value = baseline + (value/100.0)*delta
	if value < min {
		value = min
	} else if value > max {
		value = max
	}
	return time, value
}

type TransGroup struct {
	Name        string
	Transitions []Transition
}
