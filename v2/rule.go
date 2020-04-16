// Copyright (c) 2019, The Emergent Authors. All rights reserved.
// Use of this source  is governed by a BSD-style
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
	"log"
	"strings"
	"unicode"

	"github.com/goki/ki/kit"
)

const rtParen = ")"
const lftParen = "("
const wildCard = "*"
const orSym = "or"
const notSym = "not"
const xorSym = "xor"
const andSym = "and"

type LogicSymbolType int

const (
	// LogicSymInvalid
	LogicSymInvalid LogicSymbolType = iota

	//
	LogicSymOr

	//
	LogicSymNot

	//
	LogicSymXor

	//
	LogicSymAnd

	//
	LogicSymRtParen

	//
	LogicSymLftParen

	//
	LogicSymString

	LogicSymTypeN
)

//go:generate stringer -type=LogicSymbolType

var Kit_LogicSymbolType = kit.Enums.AddEnum(LogicSymTypeN, kit.NotBitFlag, nil)

// Parse
type Parser struct {
	Model   *Model
	Str     string
	Pos     int
	Symbol  string
	SymType LogicSymbolType
}

func NewParser(s string, model *Model) *Parser {
	if len(s) == 0 {
		return nil
	}
	prs := Parser{}
	prs.Model = model
	prs.Pos = 0
	prs.Str = strings.TrimSpace(s)
	prs.SymType = LogicSymInvalid
	prs.NextSymbol()
	return &prs
}

func (pars *Parser) Finished() bool {
	return pars.Pos >= len(pars.Str)
}

func IsSeparator(c string) bool {
	if c == rtParen || c == lftParen {
		return true
	}
	if unicode.IsSpace(rune(c[0])) {
		return true
	}
	return false
}

// SkipSpaces moves the index into string past white space
func (pars *Parser) SkipSpaces() {
	for !pars.Finished() && unicode.IsSpace(rune(pars.Str[pars.Pos])) {
		pars.Pos++
	}
}

// NextSymbol
func (pars *Parser) NextSymbol() {
	pars.SkipSpaces()
	pars.Symbol = ""

	if pars.Finished() {
		pars.SymType = SymInvalid
		return
	}

	c := string(pars.Str[pars.Pos])
	pars.Symbol = c
	pars.Pos++

	switch c {
	case rtParen:
		pars.SymType = SymRtParen
		break
	case lftParen:
		pars.SymType = SymLftParen
		break
	default:
		for !pars.Finished() {
			if pars.Pos < len(pars.Str) {
				cnext := string(pars.Str[pars.Pos]) // notice that Pos has been incremented already
				if IsSeparator(cnext) {
					break
				}
				pars.Symbol += cnext
				pars.Pos++
			}
		}
		if pars.Symbol == orSym {
			pars.SymType = LogicSymOr
		} else if pars.Symbol == andSym {
			pars.SymType = LogicSymAnd
		} else if pars.Symbol == notSym {
			pars.SymType = LogicSymNot
		} else if pars.Symbol == xorSym {
			pars.SymType = LogicSymXor
		} else {
			pars.SymType = LogicSymString
		}
	}
}

// GetNode returns the next boolean node
func (pars *Parser) GetBoolNode() *BoolNode {
	switch pars.SymType {

	case SymLftParen:
		{
			var bn *BoolNode
			pars.NextSymbol()

			if pars.SymType == LogicSymNot {
				// operand.
				pars.NextSymbol()
				op := pars.GetBoolNode()
				bn = op
			} else {
				// 1st operand
				var op1 *BoolNode
				op1 = pars.GetBoolNode()

				// operator.
				var op2 *BoolNode
				switch pars.SymType {
				case LogicSymOr:
					{ // 2nd operand.
						pars.NextSymbol()
						op2 = pars.GetBoolNode()
						n := BoolNode{}
						n.Type = LogicNodeOr
						n.Child1 = op1
						n.Child2 = op2
						bn = &n
					}
				case LogicSymAnd:
					{ // 2nd operand.
						pars.NextSymbol()
						op2 = pars.GetBoolNode()
						n := BoolNode{}
						n.Type = LogicNodeAnd
						n.Child1 = op1
						n.Child2 = op2
						bn = &n
					}
				case LogicSymXor:
					{ // 2nd operand.
						pars.NextSymbol()
						op2 = pars.GetBoolNode()
						n := BoolNode{}
						n.Type = LogicNodeXor
						n.Child1 = op1
						n.Child2 = op2
						bn = &n
					}
				case LogicSymNot:
					log.Println("Invalid operator")
					return nil
				default:
					log.Println("Missing operator")
					return nil
				}
			}

			if pars.SymType != SymRtParen {
				log.Println("Right parenthesis not found")
				return nil
			}
			pars.NextSymbol()
			return bn
		}

	case LogicSymString:
		{
			wild := false
			if len(pars.Symbol) >= 2 && string(pars.Symbol[len(pars.Symbol)-1]) == wildCard {
				wild = true
			}

			var name string
			if wild {
				name = pars.Symbol[0 : len(pars.Symbol)-1]
			} else {
				name = pars.Symbol
			}

			var category *Category
			posture := pars.Model.PostureTry(name)
			if posture != nil {
				category = posture.CategoryTry(name)
			} else {
				if wild {
					log.Println("Asterisk at the end of a category name")
					return nil
				}
				category = pars.Model.CategoryTry(name)
			}
			if category == nil {
				log.Printf("Could not find category: %v", name)
				return nil
			}

			pars.NextSymbol()
			bn := BoolNode{}
			bn.Type = LogicNodeTerminal
			bn.Cat = category
			bn.MatchAll = wild
			return &bn
		}
	case LogicSymOr:
		return nil
	case LogicSymNot:
		return nil
	case LogicSymXor:
		return nil
	case LogicSymAnd:
		return nil
	case SymRtParen:
		return nil
	default:
		return nil
	}
}

// Parse
func (pars *Parser) Parse() *BoolNode {
	root := pars.GetBoolNode()
	if pars.SymType != SymInvalid {
		return nil // ToDo: this doesn't make sens
	}
	return root
}

type BoolNode struct {
	Type     LogicNodeType
	Child1   *BoolNode
	Child2   *BoolNode
	Cat      *Category
	MatchAll bool
}

func (bn *BoolNode) BoolNodeEval(pos *Posture) bool {
	switch bn.Type {
	case LogicNodeOr:
		if bn.Child1 == nil || bn.Child2 == nil {
			log.Println("BoolNodeEval: Eval error - one or both of Child1 and Child2 are nil")
			return false
		}
		result := bn.Child1.BoolNodeEval(pos) || bn.Child2.BoolNodeEval(pos)
		return result
	case LogicNodeAnd:
		if bn.Child1 == nil || bn.Child2 == nil {
			log.Println("BoolNodeEval: Eval error - one or both of Child1 and Child2 are nil")
			return false
		}
		result := bn.Child1.BoolNodeEval(pos) && bn.Child2.BoolNodeEval(pos)
		return result
	case LogicNodeXor:
		if bn.Child1 == nil || bn.Child2 == nil {
			log.Println("BoolNodeEval: Eval error - one or both of Child1 and Child2 are nil")
			return false
		}
		result := bn.Child1.BoolNodeEval(pos) != bn.Child2.BoolNodeEval(pos)

		return result
	case LogicNodeNot:
		if bn.Child1 == nil {
			log.Println("BoolNodeEval: Eval error - Child1 is nil")
			return false
		}
		result := !bn.Child1.BoolNodeEval(pos)
		return result
	case LogicNodeTerminal:
		if pos.IsMemberOfCategory(bn.Cat) {
			return true
		} else if bn.MatchAll {
			return pos.Name == bn.Cat.Name+"'" // tried "\'" but got invalid escape sequence
		}
		return false
	}
	return false
}

func (r *Rule) EvalBoolExpr(postures []Posture) bool {
	if len(postures) < len(r.BoolNodes) {
		return false
	}
	if len(r.BoolNodes) == 0 {
		return false
	}
	for i := 0; i < len(r.BoolNodes); i++ {
		if !(r.BoolNodes[i].BoolNodeEval(&postures[i])) {
			return false
		}
	}
	return true
}

// EvalExpr
func (r *Rule) EvalExprSyms(tempos []float64, postures []Posture, model *Model, syms []float64) {
	var localTempos [4]float64

	model.ClearFormulaVals()

	if len(postures) >= 2 {
		pos := postures[0]
		model.FormulaVals[FormulaSymTransition1] = pos.SymTargets[1].Value
		model.FormulaVals[FormulaSymQssa1] = pos.SymTargets[2].Value
		model.FormulaVals[FormulaSymQssb1] = pos.SymTargets[3].Value

		pos = postures[1]
		model.FormulaVals[FormulaSymTransition2] = pos.SymTargets[1].Value
		model.FormulaVals[FormulaSymQssa2] = pos.SymTargets[2].Value
		model.FormulaVals[FormulaSymQssb2] = pos.SymTargets[3].Value
		localTempos[0] = tempos[0]
		localTempos[1] = tempos[1]
	} else {
		localTempos[0] = 0.0
		localTempos[1] = 0.0
	}

	if len(postures) >= 3 {
		pos := postures[2]
		model.FormulaVals[FormulaSymTransition3] = pos.SymTargets[1].Value
		model.FormulaVals[FormulaSymQssa3] = pos.SymTargets[2].Value
		model.FormulaVals[FormulaSymQssb3] = pos.SymTargets[3].Value
		localTempos[2] = tempos[2]
	} else {
		localTempos[2] = 0.0
	}

	if len(postures) >= 4 {
		pos := postures[3]
		model.FormulaVals[FormulaSymTransition4] = pos.SymTargets[1].Value
		model.FormulaVals[FormulaSymQssa4] = pos.SymTargets[2].Value
		model.FormulaVals[FormulaSymQssb4] = pos.SymTargets[3].Value
		localTempos[3] = tempos[3]
	} else {
		localTempos[3] = 0.0
	}

	model.FormulaVals[FormulaSymTempo1] = localTempos[0]
	model.FormulaVals[FormulaSymTempo2] = localTempos[1]
	model.FormulaVals[FormulaSymTempo3] = localTempos[2]
	model.FormulaVals[FormulaSymTempo4] = localTempos[3]
	model.FormulaVals[FormulaSymRd] = syms[0]
	model.FormulaVals[FormulaSymBeat] = syms[1]
	model.FormulaVals[FormulaSymMark1] = syms[2]
	model.FormulaVals[FormulaSymMark2] = syms[3]
	model.FormulaVals[FormulaSymMark3] = syms[4]

	// Execute in this order.
	if r.ExprSymEquations.Duration != nil {
		model.FormulaVals[FormulaSymRd] = model.EvalEquationFormula(r.ExprSymEquations.Duration)
	}
	if r.ExprSymEquations.Mark1 != nil {
		model.FormulaVals[FormulaSymMark1] = model.EvalEquationFormula(r.ExprSymEquations.Mark1)
	}
	if r.ExprSymEquations.Mark2 != nil {
		model.FormulaVals[FormulaSymMark2] = model.EvalEquationFormula(r.ExprSymEquations.Mark2)
	}
	if r.ExprSymEquations.Mark3 != nil {
		model.FormulaVals[FormulaSymMark3] = model.EvalEquationFormula(r.ExprSymEquations.Mark3)
	}
	if r.ExprSymEquations.Beat != nil {
		model.FormulaVals[FormulaSymBeat] = model.EvalEquationFormula(r.ExprSymEquations.Beat)
	}

	syms[0] = model.FormulaVals[FormulaSymRd]
	syms[1] = model.FormulaVals[FormulaSymBeat]
	syms[2] = model.FormulaVals[FormulaSymMark1]
	syms[3] = model.FormulaVals[FormulaSymMark2]
	syms[4] = model.FormulaVals[FormulaSymMark3]
}

//////////////////////////////////////////////////
// Rule
//////////////////////////////////////////////////

type ExprSymEquations struct {
	Duration *Equation
	Beat     *Equation
	Mark1    *Equation
	Mark2    *Equation
	Mark3    *Equation
}

type Rule struct {
	BoolExprs                 []string         `xml:"boolean-expressions>boolean-expression"`
	ParamProfileTransitions   []Transition     `xml:"parameter-profiles>parameter-transition"`
	SpecialProfileTransitions []Transition     `xml:"special-profiles>parameter-transition"`
	ExprSymEquations          ExprSymEquations `xml:"expression-symbols>symbol-equation"`
	Comment                   string           `xml:"comment"`
	BoolNodes                 []BoolNode
}

func (r *Rule) Init(nParams int) {
	r.ParamProfileTransitions = make([]Transition, nParams)
	r.SpecialProfileTransitions = make([]Transition, nParams)
}

type LogicNodeType int

const (
	// LogicSymInvalid
	LogicNodeInvalid LogicNodeType = iota

	//
	LogicNodeOr

	//
	LogicNodeNot

	//
	LogicNodeXor

	//
	LogicNodeAnd

	//
	LogicNodeTerminal

	LogicNodeTypeN
)

//go:generate stringer -type=LogicNodeType

var Kit_LogicNodeType = kit.Enums.AddEnum(LogicNodeTypeN, kit.NotBitFlag, nil)

func (r *Rule) SetExprList(exprs []string, model *Model) *[]BoolNode {
	length := len(r.BoolExprs)
	if length < 2 || length > 4 {
		log.Println("SetExprList: Invalid number of boolean expressions")
	}
	var testList []BoolNode
	for _, e := range r.BoolExprs {
		p := NewParser(e, model)
		node := p.Parse()
		testList = append(testList, *node)
	}
	// std::swap(booleanNodeList_, testBooleanNodeList);
	return &testList
}
