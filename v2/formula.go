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

type FormulaSymMap struct {
	SymMap map[string]FormulaSymbolType
}

func NewFormulaSymMap() *map[string]FormulaSymbolType {
	fs := make(map[string]FormulaSymbolType)

	fs["transition1"] = FormulaSymTransition1
	fs["transition2"] = FormulaSymTransition2
	fs["transition3"] = FormulaSymTransition3
	fs["transition4"] = FormulaSymTransition4
	fs["qssa1"] = FormulaSymQssa1
	fs["qssa2"] = FormulaSymQssa2
	fs["qssa3"] = FormulaSymQssa3
	fs["qssa4"] = FormulaSymQssa4
	fs["qssb1"] = FormulaSymQssb1
	fs["qssb2"] = FormulaSymQssb2
	fs["qssb3"] = FormulaSymQssb3
	fs["qssb4"] = FormulaSymQssb4
	fs["tempo1"] = FormulaSymTempo1
	fs["tempo2"] = FormulaSymTempo2
	fs["tempo3"] = FormulaSymTempo3
	fs["tempo4"] = FormulaSymTempo4
	fs["rd"] = FormulaSymRd
	fs["beat"] = FormulaSymBeat
	fs["mark1"] = FormulaSymMark1
	fs["mark2"] = FormulaSymMark2
	fs["mark3"] = FormulaSymMark3

	return &fs
}
