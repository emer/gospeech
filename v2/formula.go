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
	"fmt"
	"github.com/goki/ki/kit"
)

// FormulaSymbolType
type FormulaSymbolType int

const (
	FormulaSymTransition1 = iota
	FormulaSymTransition2
	FormulaSymTransition3
	FormulaSymTransition4
	FormulaSymQssa1
	FormulaSymQssa2
	FormulaSymQssa3
	FormulaSymQssa4
	FormulaSymQssb1
	FormulaSymQssb2
	FormulaSymQssb3
	FormulaSymQssb4
	FormulaSymTempo1
	FormulaSymTempo2
	FormulaSymTempo3
	FormulaSymTempo4
	FormulaSymRd
	FormulaSymBeat
	FormulaSymMark1
	FormulaSymMark2
	FormulaSymMark3
	FormulaSymTypeN
)

//go:generate stringer -type=FormulaSymbolType

var Kit_FormulaSymbolType = kit.Enums.AddEnum(FormulaSymTypeN, kit.NotBitFlag, nil)

type FormulaValueList [FormulaSymTypeN]float64
type FormulaSymMap map[FormulaSymbolType]float64

func NewFormulaSymMap(fsm *FormulaSymMap) {
	fs := make(FormulaSymMap)

	fs[FormulaSymTransition1] = 33.3333
	fs[FormulaSymTransition2] = 33.3333
	fs[FormulaSymTransition3] = 33.3333
	fs[FormulaSymTransition4] = 33.3333
	fs[FormulaSymQssa1] = 33.3333
	fs[FormulaSymQssa2] = 33.3333
	fs[FormulaSymQssa3] = 33.3333
	fs[FormulaSymQssa4] = 33.3333
	fs[FormulaSymQssb1] = 33.3333
	fs[FormulaSymQssb2] = 33.3333
	fs[FormulaSymQssb3] = 33.3333
	fs[FormulaSymQssb4] = 33.3333
	fs[FormulaSymTempo1] = 1.0
	fs[FormulaSymTempo2] = 1.0
	fs[FormulaSymTempo3] = 1.0
	fs[FormulaSymTempo4] = 1.0
	fs[FormulaSymBeat] = 33.0
	fs[FormulaSymRd] = 100.0
	fs[FormulaSymMark1] = 100.0
	fs[FormulaSymMark2] = 0.0
	fs[FormulaSymMark3] = 0.0

	fsm = &fs
}

//func NewFormulaSymbolList(n int) *FormulaSymbolList {
//	s := FormulaSymbolList{}
//	s.Symbols = make([]float64, n)
//	return &s
//}

func (fs *FormulaSymMap) Clear() {
	for _, v := range *fs {
		v = 0.0
		fmt.Printf("%f, ", v)
	}
}
