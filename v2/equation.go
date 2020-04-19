package v2

import (
	"encoding/xml"
	"log"
	"strconv"
	"strings"
	"unicode"

	"github.com/goki/ki/kit"
	"golang.org/x/exp/errors"
)

const addChar = "+"
const subChar = "-"
const multChar = "*"
const divChar = "/"
const rtParenChar = ")"
const lftParenChar = "("

type Equation struct {
	Name        string `xml:"name,attr"`
	Comment     string `xml:"comment"`
	Formula     string `xml:"formula,attr"`
	FormulaRoot *FormulaNode
}

type EqGrp struct {
	XMLName   xml.Name    `xml:"equation-group"`
	Name      string      `xml:"name,attr"`
	Equations []*Equation `xml:"equation"`
}

type SymbolType int

const (
	SymInvalid = iota

	SymAdd

	SymSub

	SymMult

	SymDiv

	SymRtParen

	SymLftParen

	SymString

	SymTypeN
)

//go:generate stringer -type=SymbolType

var Kit_SymbolType = kit.Enums.AddEnum(SymTypeN, kit.NotBitFlag, nil)

//////////////////////////////////

type NodeType int

const (
	// NodeInvalid
	NodeMinusUnary NodeType = iota

	//
	NodeAdd

	//
	NodeSub

	//
	NodeMult

	//
	NodeDiv

	//
	NodeConst

	//
	NodeSym

	NodeTypeN
)

//go:generate stringer -type=NodeType

var Kit_NodeType = kit.Enums.AddEnum(NodeTypeN, kit.NotBitFlag, nil)

type FormulaNode struct { // Parser returns the pi.Parser for this language
	Type     NodeType
	Child1   *FormulaNode
	Child2   *FormulaNode
	Symbol   FormulaSymbolType `desc:"These are dynamic variables looked up from a map"`
	ConstVal float64           `desc:"Constants in some formulas"`
}

func (fn *FormulaNode) Eval(sl *FormulaValueList) float64 {
	switch fn.Type {
	case NodeSym:
		return sl[fn.Symbol]
	case NodeConst:
		return fn.ConstVal
	case NodeMinusUnary:
		return -(fn.Eval(sl))
	case NodeAdd:
		return fn.Child1.Eval(sl) + fn.Child2.Eval(sl)
	case NodeSub:
		return fn.Child1.Eval(sl) - fn.Child2.Eval(sl)
	case NodeMult:
		return fn.Child1.Eval(sl) * fn.Child2.Eval(sl)
	case NodeDiv:
		return fn.Child1.Eval(sl) / fn.Child2.Eval(sl)
	}
	return 0.0
}

// NewFormulaNode
func NewFormulaNode(c1 *FormulaNode, c2 *FormulaNode, nodeType NodeType) *FormulaNode {
	if c1 == nil {
		log.Println("NewFormulaNode: error - Child1 is nil")
		return nil
	}
	if nodeType != NodeMinusUnary && c2 == nil {
		log.Println("NewFormulaNode: error - Child2 is nil")
		return nil
	}
	fn := new(FormulaNode)
	fn.Type = nodeType
	fn.Child1 = c1
	fn.Child2 = c2
	return fn
}

// NewFormulaSymbolNode is a special case for variables of type FormulaSymbolType
func NewFormulaSymbolNode(symbol FormulaSymbolType) *FormulaNode {
	fn := FormulaNode{}
	fn.Type = NodeSym
	fn.Symbol = symbol
	return &fn
}

// NewFormulaSymbolNode is a special case for variables of type FormulaSymbolType
func NewFormulaConstNode(val float64) *FormulaNode {
	fn := FormulaNode{}
	fn.Type = NodeConst
	fn.ConstVal = val
	return &fn
}

type FormulaNodeParser struct {
	FormulaValueList FormulaValueList
	S                string
	Pos              int
	Symbol           string
	SymbolType       SymbolType
}

func NewFormulaNodeParser(s string) *FormulaNodeParser {
	fnp := FormulaNodeParser{}
	fnp.S = strings.TrimSpace(s)
	fnp.Pos = 0
	fnp.SymbolType = SymInvalid

	if len(fnp.S) == 0 {
		log.Println("Formula expression parser error: Empty string.")
		return nil
	}
	fnp.NextSymbol()
	return &fnp
}

func (fnp *FormulaNodeParser) Finished() bool {
	return fnp.Pos >= len(fnp.S)
}

// SkipSpaces moves the index into string past white space
func (fnp *FormulaNodeParser) SkipSpaces() {
	for !fnp.Finished() && unicode.IsSpace(rune(fnp.S[fnp.Pos])) {
		fnp.Pos++
	}
}

func (fnp *FormulaNodeParser) NextSymbol() {
	fnp.SkipSpaces()

	if fnp.Finished() {
		fnp.SymbolType = SymInvalid
		return
	}

	c := string(fnp.S[fnp.Pos])
	fnp.Symbol = c
	fnp.Pos++

	switch c {
	case addChar:
		fnp.SymbolType = SymAdd
	case subChar:
		fnp.SymbolType = SymSub
	case multChar:
		fnp.SymbolType = SymMult
	case divChar:
		fnp.SymbolType = SymDiv
	case rtParenChar:
		fnp.SymbolType = SymRtParen
	case lftParenChar:
		fnp.SymbolType = SymLftParen
	default:
		fnp.SymbolType = SymString
		c = string(fnp.S[fnp.Pos]) // notice that Pos has been incremented already
		for !fnp.Finished() && !IsSeparator(string(c)) {
			fnp.Symbol += string(c)
			fnp.Pos++
			if fnp.Pos >= len(fnp.S) {
				break
			}
			c = string(fnp.S[fnp.Pos]) // notice that Pos has been incremented already
		}
	}
}

// Parse Factor -- FACTOR -> "(" EXPRESSION ")" | SYMBOL | CONST | ADD_OP FACTOR
func (fnp *FormulaNodeParser) ParseFactor() *FormulaNode {
	switch fnp.SymbolType {
	case SymLftParen: // expression
		fnp.NextSymbol()
		res := fnp.ParseExpr()
		if fnp.SymbolType != SymRtParen {
			//return nil, errors.New("ParseFactor: Right parenthesis not found")
			return nil
		}
		fnp.NextSymbol()
		return res
	case SymAdd: // unary plus
		fnp.NextSymbol()
		return fnp.ParseFactor()
	case SymSub: // unary minus
		fnp.NextSymbol()
		op := NewFormulaNode(fnp.ParseFactor(), nil, NodeMinusUnary)
		return op
	case SymString: // const / symbol
		tmpSym := fnp.Symbol
		fnp.NextSymbol()

		tempMap := NewFormulaSymMap()
		for k, v := range *tempMap {
			if tmpSym == k {
				return NewFormulaSymbolNode(v)
			}
		}
		// so not a symbol we know
		f, err := strconv.ParseFloat(tmpSym, 64)
		if err != nil {
			panic(errors.New("Equation.ParseFactor() : ParseFloat failed!"))
		}
		return NewFormulaConstNode(f)
	case SymRtParen:
		panic(errors.New("Equation.ParseFactor() : Unexpected right Paren Char!"))
		return nil
	case SymMult:
		panic(errors.New("Equation.ParseFactor() : Unexpected multiplier char!"))
		return nil
	case SymDiv:
		panic(errors.New("Equation.ParseFactor() : Unexpected div char!"))
		return nil
	default:
		panic(errors.New("Equation.ParseFactor() : invalid symbol!"))
		return nil
	}
	return nil
}

// ParseTerm TERM -> FACTOR { MULT_OP FACTOR }
func (fnp *FormulaNodeParser) ParseTerm() *FormulaNode {
	term1 := fnp.ParseFactor()

	symType := fnp.SymbolType
	for symType == SymMult || symType == SymDiv {
		fnp.NextSymbol()
		term2 := fnp.ParseFactor()
		var expr *FormulaNode
		if symType == SymMult {
			op := NewFormulaNode(term1, term2, NodeMult)
			expr = op
		} else {
			op := NewFormulaNode(term1, term2, NodeDiv)
			expr = op

		}
		temp := term1
		term1 = expr
		expr = temp
		symType = fnp.SymbolType
	}

	return term1
}

// ParseExpr  EXPRESSION -> TERM { ADD_OP TERM }
func (fnp *FormulaNodeParser) ParseExpr() *FormulaNode {
	term1 := fnp.ParseTerm()

	symType := fnp.SymbolType
	for symType == SymAdd || symType == SymSub {
		fnp.NextSymbol()
		term2 := fnp.ParseTerm()

		var expr *FormulaNode
		if symType == SymAdd {
			op := NewFormulaNode(term1, term2, NodeAdd)
			expr = op
		} else {
			op := NewFormulaNode(term1, term2, NodeSub)
			expr = op
		}
		temp := term1
		term1 = expr
		expr = temp
		symType = fnp.SymbolType
	}

	return term1
}

// Parse
func (fnp *FormulaNodeParser) Parse() *FormulaNode {
	formulaRoot := fnp.ParseExpr()
	if fnp.SymbolType != SymInvalid { // hmmm, seems backwards
		log.Println("Parse: Invalid text")
		return nil
	}
	return formulaRoot
}

func (eq *Equation) SetFormula(formula string) *FormulaNode {
	np := NewFormulaNodeParser(formula)
	eq.Formula = formula
	root := np.Parse()
	return root
}

func (eq *Equation) EvalFormula(sl *FormulaValueList) float64 {
	if eq.FormulaRoot == nil {
		log.Println("EvalFormula: root is nil")
		return 0.0
	}
	return eq.FormulaRoot.Eval(sl)
}
