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
	Model *Model
	Str string
	Pos int
	Symbol string
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
		cnext := string(pars.Str[pars.Pos])  // notice that Pos has been incremented already
		for !pars.Finished() && !IsSeparator(cnext)  {
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
func (pars *Parser) GetNode() (node *Node) {
	switch pars.SymType {

	case SymLftParen:
	{
		//var p *Node

		pars.NextSymbol()
		if pars.SymType == LogicSymNot {
			// operand.
			pars.NextSymbol()
			var op *Node
			op = pars.GetNode()
			node = op
		} else {
			// 1st operand
			var op1 *Node
			op1 = pars.GetNode()

			// operator.
			var op2 *Node
			switch pars.SymType {
			case LogicSymOr:
			{	// 2nd operand.
				pars.NextSymbol()
				op2 = pars.GetNode()
				newExpr :=
				// p.reset(new RuleBooleanOrExpression(std::move(op1), std::move(op2)))
				break;
			}
			case LogicSymAnd:
			{	// 2nd operand.
				pars.NextSymbol()
				op2 = pars.GetNode()
				node = RuleAndExpression(op1, op2)

			}
			case LogicSymXor:
			{	// 2nd operand.
				pars.NextSymbol()
				op2  = pars.GetNode()
				node = RuleXorExpression(op1, op2)

			}
			case LogicSymNot:
				log.Println("Invalid operator")
				return nil
			default:
				log.Println("Missing operator")
				return nil
			}
		}

		if (pars.SymType != SymRtParen) {
			log.Println("Right parenthesis not found")
			return nil
		}
		pars.NextSymbol()
		return nil
	}

	case LogicSymString:
	{
		wild := false;
		if len(pars.Symbol) >= 2 && pars.Symbol[len(pars.Symbol - 1)] == wildCard {
			wild = true;
		}

		var name string
		if wild {
			name = pars.Symbol.substr(0, pars.Symbol.size() - 1)
		} else {
			name = pars.Symbol;
		}

		var category Category
		posture = pars.Model.PostureNameTry(name)
		if posture != nil {
			category = posture.CategoryNameTry(name)
		} else {
			if wild {
				log.Println("Asterisk at the end of a category name")
				return nil
			}
			category = pars.Model.CategoryNameTry(name)
		}
		if category == nil {
			log.Printf("Could not find category: %v", name)
			return nil
		}

 		nextSymbol()
		nt := NewNode(NodeTerminal, nil, nil)
		nt.Category = Category
		nt.Wild = wilcard
		return &nt
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
func (pars *Parser) Parse() *Node {
	root := pars.GetNode()
	if (root.NodeType != SymInvalid) {
		return nil // ToDo: this doesn't make sens
	}
	return root
}

// EvalExpr
func (r *Rule) EvalExpr(tempos []float64, postures []Posture, model *Model, syms []float64) {
	var localTempos []float64

	model.Formula.Clear()

	if len(postures) >= 2 {
		pos := postures[0]
		model.Formula[Transition1] = pos.GetSymbolTarget(1)
		model.Formula[Qssa1] = pos.GetSymbolTarget(2)
		 model.Formula[Qssb1] = pos.GetSymbolTarget(3)

		pos := postures[1]
		model.Formula[Transition2] = pos.GetSymbolTarget(1)
		model.Formula[Qssa2] = Posture.GetSymbolTarget(2)
		 model.Formula[Qssb2] = pos.GetSymbolTarget(3)
		localTempos[0] = tempos[0]
		localTempos[1] = tempos[1]
	} else {
		localTempos[0] = 0.0
		localTempos[1] = 0.0
	}

	if len(postures) >= 3 {
				pos := postures[2]
	model.Formula[Transition3] = pos.GetSymbolTarget(1)
		model.Formula[Qssa3] = pos.GetSymbolTarget(2)
		 model.Formula[Qssb3] = pos.GetSymbolTarget(3)
		localTempos[2] = tempos[2]
	} else {
		localTempos[2] = 0.0
	}

	if len(postures) >= 4 {
				pos := postures[3]
	model.Formula[Transition4] = pos.GetSymbolTarget(1)
		model.Formula[Qssa4] = pos.GetSymbolTarget(2)
		 model.Formula[Qssb4] = pos.GetSymbolTarget(3)
		localTempos[3] = tempos[3]
	} else {
		localTempos[3] = 0.0
	}

	model.Formula.Symbols[FormulaSymTempo1] = localTempos[0]
	model.Formula.Symbols[FormulaSymTempo2] = localTempos[1]
	model.Formula.Symbols[FormulaSymTempo3] = localTempos[2]
	model.Formula.Symbols[FormulaSymTempo4] = localTempos[3]
	model.Formula.Symbols[FormulaSymRd]    = syms[0]
	model.Formula.Symbols[FormulaSymBeat]  = syms[1]
	model.Formula.Symbols[FormulaSymMark1] = syms[2]
	model.Formula.Symbols[FormulaSymMark2] = syms[3]
	model.Formula.Symbols[FormulaSymMark3] = syms[4]


	// Execute in this order.
	if (r.ExprSymEquations) {
		model.setFormulaSymbolValue(FormulaSymRd, model.evalEquationFormula(*exprSymbolEquations_.ruleDuration));
	}
	if (exprSymbolEquations_.mark1) {
		model.setFormulaSymbolValue(FormulaSymMark1, model.evalEquationFormula(*exprSymbolEquations_.mark1));
	}
	if (exprSymbolEquations_.mark2) {
		model.setFormulaSymbolValue(FormulaSymMark2, model.evalEquationFormula(*exprSymbolEquations_.mark2));
	}
	if (exprSymbolEquations_.mark3) {
		model.setFormulaSymbolValue(FormulaSymMark3, model.evalEquationFormula(*exprSymbolEquations_.mark3));
	}
	if (exprSymbolEquations_.beat) {
		model.setFormulaSymbolValue(FormulaSymBeat, model.evalEquationFormula(*exprSymbolEquations_.beat));
	}

	syms[0] = model.Formula.Syms[FormulaSymRd]
	syms[1] = model.Formula.Syms[FormulaSymBeat]
	syms[2] = model.Formula.Syms[FormulaSymMark1]
	syms[3] = model.Formula.Syms[FormulaSymMark2]
	syms[4] = model.Formula.Syms[FormulaSymMark3]
}


//////////////////////////////////////////////////
// Rule
//////////////////////////////////////////////////

type ExprSymEquation struct {
	Duration *Equation
	Beat *Equation
	Mark1 *Equation
	Mark2 *Equation
	Mark3 *Equation	
}

type Rule struct {
	BoolExprs []string
	ParamProfileTransitions []Transition
	SpecialProfileTransitions []Transition
	ExprSymEquations []ExprSymEquation
	Comment string
	Nodes []Node
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

type Node struct {
	NodeType LogicNodeType
	Child1 *Node // all but terminal
	Child2 *Node // for and, or, xor
	Category *Category // only for terminal node
	wildCard bool // only for terminal node
}

func NewNode(typ LogicNodeType, c1, c2 *Node) *Node {
	ex := &Node{}
	ex.NodeType = typ
	ex.Child1 = c1
	ex.Child2 = c2
	return ex
}

func (nd *Node) Eval(posture *Posture) (result bool, err error) {
	var r1 = false
	var r2 = false

	switch nd.NodeType {
	case LogicNodeAnd:
		if nd.Child1 == nil || nd.Child2 == nil {
			errors.New("Eval error: One or more of nodes children were nil")
		}
		r1, err = nd.Child1.Eval(posture)
		r2, err = nd.Child2.Eval(posture)
		return r1 && r2, nil

	case LogicNodeOr:
		if nd.Child1 == nil || nd.Child2 == nil {
			errors.New("Eval error: One or more of nodes children were nil")
		}
		r1, err = nd.Child1.Eval(posture)
		r2, err = nd.Child2.Eval(posture)
		return r1 || r2, nil
		
	case LogicNodeXor:
		if nd.Child1 == nil || nd.Child2 == nil {
			errors.New("Eval error: One or more of nodes children were nil")
		}
		r1, err = nd.Child1.Eval(posture)
		r2, err = nd.Child2.Eval(posture)
		return r1 != r2, nil
		
	case LogicNodeNot:
		if nd.Child1 == nil {
			errors.New("Eval error: chile1 was nil")
		}
		r1, err = nd.Child1.Eval(posture)
		return !r1, nil
		
	case LogicNodeTerminal:
		if posture.IsMemberOfCategory {
			return true, nil
		} else if nd.wildCard {
			return posture.Name == (nd.Category + "\'"), nil
		} else {
			return false, nil
		}
	}
}

// ToDo: Not right!
func (r *Rule)SetExprList(exprs []string, model *Model) error {
	length := len(exprs)
	if length < 2 || length > 4 {
		return errors.New("Invalid number of boolean expressions")
	}
	var testList []Nodes
	for _, e := range exprs {
		p := Parser{}
		p.Str = e
		p.Model = model
		node := p.Parse()
		testList = append(testList, node)
	}
	r.BoolExprs = exprs
	// std::swap(booleanNodeList_, testBooleanNodeList);	
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

