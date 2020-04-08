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
	"encoding/xml"
	"errors"
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
	pars.Pos++
	//pars.SymType = c

	switch c {
	case rtParen:
		pars.SymType = SymRtParen
		break
	case lftParen:
		pars.SymType = SymLftParen
		break
	default:
		cnext := string(pars.Str[pars.Pos]) // notice that Pos has been incremented already
		for !pars.Finished() && !IsSeparator(cnext) {
			pars.Symbol += cnext
			pars.Pos++
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
func (pars *Parser) GetRuleNode() *RuleBooleanNode {
	switch pars.SymType {

	case SymLftParen:
		{
			var p *RuleBooleanNode

			pars.NextSymbol()

			if pars.SymType == LogicSymNot {
				// operand.
				pars.NextSymbol()
				op := pars.GetRuleNode()
				p = op
			} else {
				// 1st operand
				var op1 *RuleBooleanNode
				op1 = pars.GetRuleNode()

				// operator.
				var op2 *RuleBooleanNode
				switch pars.SymType {
				case LogicSymOr:
					{ // 2nd operand.
						pars.NextSymbol()
						op2 = pars.GetRuleNode()
						ex := RuleOrExpr{}
						ex.Child1 = op1
						ex.Child2 = op2
						p = &ex.RuleBooleanNode
					}
				case LogicSymAnd:
					{ // 2nd operand.
						pars.NextSymbol()
						op2 = pars.GetRuleNode()
						ex := RuleAndExpr{}
						ex.Child1 = op1
						ex.Child2 = op2
						p = &ex.RuleBooleanNode
					}
				case LogicSymXor:
					{ // 2nd operand.
						pars.NextSymbol()
						op2 = pars.GetRuleNode()
						ex := RuleXorExpr{}
						ex.Child1 = op1
						ex.Child2 = op2
						p = &ex.RuleBooleanNode
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
			return p
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
			nt := RuleTerminalExpr{}
			nt.Cat = category
			nt.MatchAll = wild
			return &nt.RuleBooleanNode
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
func (pars *Parser) Parse() *RuleBooleanNode {
	root := pars.GetRuleNode()
	if pars.SymType != SymInvalid {
		return nil // ToDo: this doesn't make sens
	}
	return root
}

func RuleNodeEval(node *RuleBooleanNode) bool {
	return true
}

type RuleBooleanNode interface {
	Eval(pos *Posture) bool
}

type RuleAndExpr struct {
	RuleBooleanNode
	Child1 *RuleBooleanNode
	Child2 *RuleBooleanNode
}

func (ex *RuleAndExpr) Eval(pos *Posture) bool {
	if ex.Child1 == nil || ex.Child2 == nil {
		log.Println("RuleAndExpr: Eval error - one or both of Child1 and Child2 are nil")
		return false
	}
	result := RuleNodeEval(ex.Child1) && RuleNodeEval(ex.Child2)
	return result
}

type RuleXorExpr struct {
	RuleBooleanNode
	Child1 *RuleBooleanNode
	Child2 *RuleBooleanNode
}

func (ex *RuleXorExpr) Eval(pos *Posture) bool {
	if ex.Child1 == nil || ex.Child2 == nil {
		log.Println("RuleAndExpr: Eval error - one or both of Child1 and Child2 are nil")
		return false
	}
	result := RuleNodeEval(ex.Child1) != RuleNodeEval(ex.Child2)
	return result
}

type RuleOrExpr struct {
	RuleBooleanNode
	Child1 *RuleBooleanNode
	Child2 *RuleBooleanNode
}

func (ex *RuleOrExpr) Eval(pos *Posture) bool {
	if ex.Child1 == nil || ex.Child2 == nil {
		log.Println("RuleOrExpr: Eval error - one or both of Child1 and Child2 are nil")
		return false
	}
	result := RuleNodeEval(ex.Child1) || RuleNodeEval(ex.Child2)
	return result
}

type RuleNotExpr struct {
	RuleBooleanNode
	Child *RuleBooleanNode
}

func (ex *RuleNotExpr) Eval(pos *Posture) bool {
	if ex.Child == nil {
		log.Println("RuleAndExpr: Eval error - one or both of Child1 and Child2 are nil")
		return false
	}
	result := !RuleNodeEval(ex.Child)
	return result
}

type RuleTerminalExpr struct {
	RuleBooleanNode
	Cat      *Category
	MatchAll bool
}

func (ex *RuleTerminalExpr) Eval(pos *Posture) bool {
	if pos.IsMemberOfCategory(ex.Cat) {
		return true
	} else if ex.MatchAll {
		return pos.Name == ex.Cat.Name+"'" // tried "\'" but got invalid escape sequence
	}
	return false
}

func (r *Rule) EvalRuleExpr(postures []Posture) bool {
	if len(postures) < len(r.RuleNodes) {
		return false
	}
	if len(r.RuleNodes) == 0 {
		return false
	}
	for i := 0; i < len(r.RuleNodes); i++ {
		if !(r.RuleNodes[i].Eval(&postures[i])) {
			return false
		}
	}
	return true
}

//bool
//Rule::evalBooleanExpression(const Posture& posture, unsigned int expressionIndex) const
//{
//if (expressionIndex >= booleanNodeList_.size()) return false;
//
//return booleanNodeList_[expressionIndex]->eval(posture);
//}

// EvalExpr
func (r *Rule) EvalExprSyms(tempos []float64, postures []Posture, model *Model, syms []float64) {
	var localTempos []float64

	model.FormulaSymbols.Clear()

	if len(postures) >= 2 {
		pos := postures[0]
		model.FormulaSymbols.CodeMap[FormulaSymTransition1] = pos.SymTargets[1]
		model.FormulaSymbols.CodeMap[FormulaSymQssa1] = pos.SymTargets[2]
		model.FormulaSymbols.CodeMap[FormulaSymQssb1] = pos.SymTargets[3]

		pos = postures[1]
		model.FormulaSymbols.CodeMap[FormulaSymTransition2] = pos.SymTargets[1]
		model.FormulaSymbols.CodeMap[FormulaSymQssa2] = pos.SymTargets[2]
		model.FormulaSymbols.CodeMap[FormulaSymQssb2] = pos.SymTargets[3]
		localTempos[0] = tempos[0]
		localTempos[1] = tempos[1]
	} else {
		localTempos[0] = 0.0
		localTempos[1] = 0.0
	}

	if len(postures) >= 3 {
		pos := postures[2]
		model.FormulaSymbols.CodeMap[FormulaSymTransition3] = pos.SymTargets[1]
		model.FormulaSymbols.CodeMap[FormulaSymQssa3] = pos.SymTargets[2]
		model.FormulaSymbols.CodeMap[FormulaSymQssb3] = pos.SymTargets[3]
		localTempos[2] = tempos[2]
	} else {
		localTempos[2] = 0.0
	}

	if len(postures) >= 4 {
		pos := postures[3]
		model.FormulaSymbols.CodeMap[FormulaSymTransition4] = pos.SymTargets[1]
		model.FormulaSymbols.CodeMap[FormulaSymQssa4] = pos.SymTargets[2]
		model.FormulaSymbols.CodeMap[FormulaSymQssb4] = pos.SymTargets[3]
		localTempos[3] = tempos[3]
	} else {
		localTempos[3] = 0.0
	}

	model.FormulaSymbols.CodeMap[FormulaSymTempo1] = localTempos[0]
	model.FormulaSymbols.CodeMap[FormulaSymTempo2] = localTempos[1]
	model.FormulaSymbols.CodeMap[FormulaSymTempo3] = localTempos[2]
	model.FormulaSymbols.CodeMap[FormulaSymTempo4] = localTempos[3]
	model.FormulaSymbols.CodeMap[FormulaSymRd] = syms[0]
	model.FormulaSymbols.CodeMap[FormulaSymBeat] = syms[1]
	model.FormulaSymbols.CodeMap[FormulaSymMark1] = syms[2]
	model.FormulaSymbols.CodeMap[FormulaSymMark2] = syms[3]
	model.FormulaSymbols.CodeMap[FormulaSymMark3] = syms[4]

	// Execute in this order.
	if r.ExprSymEquations.Duration != nil {
		model.FormulaSymbols.CodeMap[FormulaSymRd] = model.EvalEquationFormul(r.ExprSymEquations.Duration)
	}
	if r.ExprSymEquations.Mark1 != nil {
		model.FormulaSymbols.CodeMap[FormulaSymMark1] = model.EvalEquationFormul(r.ExprSymEquations.Mark1)
	}
	if r.ExprSymEquations.Mark2 != nil {
		model.FormulaSymbols.CodeMap[FormulaSymMark2] = model.EvalEquationFormul(r.ExprSymEquations.Mark2)
	}
	if r.ExprSymEquations.Mark3 != nil {
		model.FormulaSymbols.CodeMap[FormulaSymMark3] = model.EvalEquationFormul(r.ExprSymEquations.Mark3)
	}
	if r.ExprSymEquations.Beat != nil {
		model.FormulaSymbols.CodeMap[FormulaSymBeat] = model.EvalEquationFormul(r.ExprSymEquations.Beat)
	}

	syms[0] = model.FormulaSymbols.CodeMap[FormulaSymRd]
	syms[1] = model.FormulaSymbols.CodeMap[FormulaSymBeat]
	syms[2] = model.FormulaSymbols.CodeMap[FormulaSymMark1]
	syms[3] = model.FormulaSymbols.CodeMap[FormulaSymMark2]
	syms[4] = model.FormulaSymbols.CodeMap[FormulaSymMark3]
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
	XMLName                   xml.Name `xml:"rule"`
	BoolExprs                 []string `xml:"boolean-expression"`
	ParamProfileTransitions   []Transition
	SpecialProfileTransitions []Transition
	ExprSymEquations          ExprSymEquations
	Comment                   string
	RuleNodes                 []RuleBooleanNode
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

// ToDo: Not right!
func (r *Rule) SetExprList(exprs []string, model *Model) error {
	length := len(exprs)
	if length < 2 || length > 4 {
		return errors.New("Invalid number of boolean expressions")
	}
	var testList []RuleBooleanNode
	for _, e := range exprs {
		p := Parser{}
		p.Str = e
		p.Model = model
		node := p.Parse()
		testList = append(testList, *node)
	}
	r.BoolExprs = exprs
	// std::swap(booleanNodeList_, testBooleanNodeList);
	return nil
}

// Rule::setBooleanExpressionList(const std::vector<std::string>& exprList, const Model& model)
// {
// 	unsigned int size = exprList.size();
// 	if (size < 2U || size > 4U) {
// 		THROW_EXCEPTION(InvalidParameterException, "Invalid number of boolean expressions: " << size << '.');
// 	}
//
// 	RuleBooleanNodeList testBooleanNodeList;
//
// 	for (unsigned int i = 0; i < size; ++i) {
// 		Parse p(exprList[i], model);
// 		testBooleanNodeList.push_back(p.parse());
// 	}
//
// 	booleanExpressionList_ = exprList;
// 	std::swap(booleanNodeList_, testBooleanNodeList);
// }
