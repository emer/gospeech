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
	"bufio"
	"fmt"
	"gonum.org/v1/gonum/mathext/prng"
	"io"
	"log"
	"math"
	"math/rand"
	"os"
	"strings"

	"github.com/goki/ki/kit"
)

const intonationConfig = "/intonation"

const DiPhone = 2
const TriPhone = 3
const TetraPhone = 4

// PhoneType
//type PhoneType int
//
//const (
//	// DiPhone
//	DiPhone PhoneType = iota + 2
//
//	// TriPhone
//	TriPhone
//
//	// TetraPhone
//	TetraPhone
//
//	PhoneTypeN
//)
//
////go:generate stringer -type=PhoneType
//
//var Kit_PhoneType = kit.Enums.AddEnum(PhoneTypeN, kit.NotBitFlag, nil)

// ToneType
type ToneType int

const (
	// ToneStatement
	ToneStatement ToneType = iota

	// ToneExclamation
	ToneExclamation

	// ToneQuestion
	ToneQuestion

	// ToneContinuation
	ToneContinuation

	// ToneSemicolon
	ToneSemicolon

	ToneTypeN
)

//go:generate stringer -type=ToneType

var Kit_ToneType = kit.Enums.AddEnum(ToneTypeN, kit.NotBitFlag, nil)

const intonationConfigFileName = "/intonation"
const eps = 1.0e-6

var invalidEvent = math.Inf(1) // positive infinity

type Event struct {
	Events []float64
	Size   int
	Time   int
	Flag   int
}

func (ev *Event) Defaults() {
	ev.Size = 36
	ev.Time = 0.0
	ev.Flag = 0
	ev.Events = make([]float64, ev.Size)
	for i := 0; i < len(ev.Events); i++ {
		ev.Events[i] = invalidEvent
	}
}

func (ev *Event) Value(idx int) float64 {
	return ev.Events[idx]
}

func (ev *Event) SetValue(v float64, idx int {
	if idx < 0 {
		return
	}
	ev.Events[idx] = v
}

type PostureData struct {
	Posture   *Posture
	Syllable  int
	Onset     float64
	RuleTempo float64
}

func (pd *PostureData) Defaults( {
	pd.Posture = nil
	pd.Syllable = 0
	pd.Onset = 0.0
	pd.RuleTempo = 0.0
}

type Foot struct {
	Onset1 float64
	Onset2 float64
	Tempo  float64
	Start  int
	End    int
	Marked bool
	Last   int
}

func (ft *Foot) Defaults( {
	ft.Onset1 = 0.0
	ft.Onset2 = 0.0
	ft.Tempo = 1.0
	ft.Start = 0
	ft.End = 0
	ft.Marked = false
	ft.Last = 0
}

type ToneGroup struct {
	StartFoot int
	EndFoot   int
	Type      ToneType
}

func (tn *ToneGroup) Defaults( {
	tn.StartFoot = 0
	tn.EndFoot = 0
	tn.Type = 0
}

type RuleData struct {
	Number       int
	FirstPosture int
	LastPosture  int
	Duration     float64
	Beat         float64
}

func (rl *RuleData) Defaults( {
	rl.Number = 0
	rl.FirstPosture = 0
	rl.LastPosture = 0
	rl.Duration = 0.0
	rl.Beat = 0.0
}

type Sequence struct {
	Model *Model
	Events []Event

	ZeroRef                  int
	ZeroIdx                  int
	Duration                 int
	TimeQuant                int
	MacroFlag                int
	MicroFlag                int
	DriftFlag                int
	SmoothInton              int
	PitchMean                float64
	GlobalTempo              float64
	Multiplier               float64
	IntonationParams         []float64
	PostureDatum             []PostureData
	PostureTempos            []float64
	CurPosture               int
	Feet                     []Foot
	CurFoot                  int
	ToneGroups               []ToneGroup
	CurToneGroup             int
	RuleDatum                []RuleData
	CurRule                  int
	Min                      [16]float64
	Max                      [16]float64
	IntonationPts            []IntonationPt
	Drift                    Drift
	TgUseRandom              bool
	IntonRandom              float64
	TgParams                 [][]float64
	TgCount                  []int
	UseFixedIntonationParams bool
	FixedIntonationParams    []float64
	RadiusCoef               []float64 // TRM::Tube::TOTAL_REGIONS

	// std::random_device randDev_;
	 randSrc prng.MT19937_64
	// std::uniform_real_distribution<> randDist_;

}

func NewSequence(config string, model *Model) *Sequence {
	seq := Sequence{}

	seq.MacroFlag = 0
	seq.MicroFlag = 0
	seq.DriftFlag = 0
	seq.SmoothInton = 1
	seq.GlobalTempo = 1.0
	seq.UseFixedIntonationParams = false

	seq.Model = model
	seq.TgParams = make([][]float64, ToneTypeN)
	seq.InitToneGroups(config)

	seq.FixedIntonationParams = make([]float64, 10) // why 10?
	for i, _ := range seq.FixedIntonationParams {
		seq.FixedIntonationParams[i] = 1.0
	}
	seq.RadiusCoef = make([]float64, 8) // TRM::Tube::TOTAL_REGIONS
	for i, _ := range seq.RadiusCoef {
		seq.RadiusCoef[i] = 1.0
	}
	seq.TgCount = make([]int, 5)
	return &seq
}

func (seq *Sequence) Defaults() {
	seq.MacroFlag = 0
	seq.MicroFlag = 0
	seq.DriftFlag = 0
	seq.SmoothInton = 1
	seq.GlobalTempo = 1.0
	seq.UseFixedIntonationParams = false
}

func (seq *Sequence) Init(config string, model *Model) {

}

func (seq *Sequence) SetIntonationParams(pitch, pretonicRange, pretonicLift, tonicRange, tonicMovement float64 {
	seq.FixedIntonationParams[1] = pitch
	seq.FixedIntonationParams[2] = pretonicRange
	seq.FixedIntonationParams[3] = pretonicLift
	seq.FixedIntonationParams[5] = tonicRange
	seq.FixedIntonationParams[6] = tonicMovement
}

func (seq *Sequence) SetRadiusCoefs(values []float64) {
	for i := 0; i < 8; i++ { // TRM::Tube::TOTAL_REGIONS
		seq.RadiusCoef[i] = values[i]
	}
}

func (seq *Sequence) ParseGroups(idx int, count int, fp *os.File) {
	seq.TgParams[idx] = make([]float64, 10 * count)
	f := bufio.NewReader(fp)
	for i := 0; i < count; i++ {
		for {
			line, isP, err := f.ReadLine()
			if err == io.EOF {
				break
			}
			if isP == true {
				log.Println("ParseGroups: partial line read, will likely be a problem")
			}
			seq.TgParams[idx] = make([]float64, i*10)
			temp := seq.TgParams
			fmt.Sscanf(string(line), " %f %f %f %f %f %f %f %f %f %f",
				&temp[0], &temp[1], &temp[2], &temp[3], &temp[4],
				&temp[5], &temp[6], &temp[7], &temp[8], &temp[9])
		}
	}
}

func (seq *Sequence) InitToneGroups(configDirPath string) error {
	var fp *os.File
	count := 0

	path := configDirPath + intonationConfig
	fp, err := os.Open(path) // For read access.
	if err != nil {
		return err
	}

	f := bufio.NewReader(fp)
	for {
		line, isP, err := f.ReadLine()
		if err == io.EOF {
			break
		}
		if isP == true {
			log.Println("ParseGroups: partial line read, will likely be a problem")
		}
		if (line[0] == '#') || (line[0] == ' ') {
			// Skip.
		} else if strings.HasPrefix(string(line), "TG") {
			fmt.Sscanf(string(line[2]), " %d", seq.TgCount[count])
			seq.ParseGroups(count, seq.TgCount[count], fp)
			count++
		} else if strings.HasPrefix(string(line), "RANDOM") {
			fmt.Sscanf(string(line[6]), " %f", seq.IntonRandom)
		}
	}
	fp.Close()
	return nil
}

func (seq *Sequence) GetBeatAtIndex(idx int) float64 {
	if idx > seq.CurRule {
		return 0.0
	} else {
		return seq.RuleDatum[idx].Beat
	}
}

func (seq *Sequence) NewPosture() {
	if seq.PostureDatum[seq.CurPosture].Posture != nil {
		pd := PostureData{}
		seq.PostureDatum = append(seq.PostureDatum, pd)
		seq.PostureTempos = append(seq.PostureTempos, 1.0)
		seq.CurPosture++;
	}
	seq.PostureTempos[seq.CurPosture] = 1.0;
}

func (seq *Sequence) NewPostureWithObject(p *Posture) {
	if seq.PostureDatum[seq.CurPosture].Posture != nil {
		pd := PostureData{}
		seq.PostureDatum = append(seq.PostureDatum, pd)
		seq.PostureTempos = append(seq.PostureTempos, 1.0)
		seq.CurPosture++;
	}
	seq.PostureTempos[seq.CurPosture] = 1.0;
	seq.PostureDatum[seq.CurPosture].RuleTempo = 1.0;
	seq.PostureDatum[seq.CurPosture].Posture = p;
}

func (seq *Sequence) replaceCurrentPostureWith(p *Posture) {
	if seq.PostureDatum[seq.CurPosture].Posture != nil {
		seq.PostureDatum[seq.CurPosture].Posture = p;
	} else {
		seq.PostureDatum[seq.CurPosture - 1].Posture = p;
	}
}

func (seq *Sequence) NewFoot() {
	if seq.CurPosture == 0 {
		return
	}

	seq.Feet[seq.CurFoot].End = seq.CurPosture
	seq.CurFoot++
	seq.NewPosture()

	f := Foot{}
	f.Start = seq.CurPosture
	f.End = -1
	f.Tempo = 1.0
	seq.Feet = append(seq.Feet, f)
}

func (seq *Sequence) NewToneGroup() {
	if seq.CurFoot == 0 {
		return
	}

	seq.ToneGroups[seq.CurToneGroup].EndFoot = seq.CurFoot
	seq.CurToneGroup++
	seq.NewFoot()

	tg := ToneGroup{}
	tg.StartFoot = seq.CurFoot
	tg.EndFoot = -1
	seq.ToneGroups = append(seq.ToneGroups, tg)
}

// ToDo: check for correctness - especially later code doing actual insert!
// InsertEvent
func (seq *Sequence) InsertEvent(n int, t, v float64) *Event {
	t = t * seq.Multiplier;
	if t < 0.0 {
		return nil
	}
	if t > float64(seq.Duration + seq.TimeQuant) {
		return nil
	}
	
	time := seq.ZeroRef + int(t)
	time = (time >> 2) << 2;
	//if (tempTime % timeQuantization) != 0 { //commented out in C++ version
	//	tempTime++;
	//}
	
	if len(seq.Events) == 0 {
		ev := Event{}
		ev.Time = time
		if n >= 0 {
			ev.SetValue(v, n)
		}

		seq.Events = append(seq.Events, ev)
		return &seq.Events[len(seq.Events) - 1]
	}

	var i int
	for i = len(seq.Events) - 1; i >= seq.ZeroIdx; i-- {
		if seq.Events[i].Time == time {
			if n >= 0 {
				seq.Events[i].SetValue(v, n)
			}
			return &seq.Events[i]
		}
		if seq.Events[i].Time < time {
			ev := Event{}
			ev.Time = time
			if n >= 0 {
				ev.SetValue(v, n)
			}
			// Insert into slice
			seq.Events = append(seq.Events, ev) // can reuse ev, will be overwritten
			copy(seq.Events[i + 2:], seq.Events[i + 1:])
			seq.Events[i + 1] = ev
			return &seq.Events[i + 1]
		}
	}

	ev := Event{}
	ev.Time = time
	if n >= 0 {
		ev.SetValue(v, n)
	}
	// Insert into slice
	seq.Events = append(seq.Events, ev) // can reuse ev, will be overwritten
	copy(seq.Events[i + 2:], seq.Events[i + 1:])
	seq.Events[i + 1] = ev
	return &seq.Events[i + 1]
}

func (seq *Sequence) SetZeroRef(nv int) {
	seq.ZeroRef = nv
	seq.ZeroIdx = 0

	if len(seq.Events) == 0 {
		return
	}

	for i := len(seq.Events) - 1; i >= 0; i-- {
		if seq.Events[i].Time < nv {
			seq.ZeroIdx = i;
			return;
		}
	}
}

func (seq *Sequence) SlopeRatioEvents(evIdx int, sr *SlopeRatio, baseline, paramDelta, min, max float64) float64 {
	sum := 0.0
	var pointTime float64
	var pointValue float64

	pointTime, pointValue = PointData(sr.Points[0], seq.Model)
	baseTime := pointTime;
	startValue := pointValue;

	l := len(sr.Points)
	pointTime, pointValue = PointData(sr.Points[l-1], seq.Model)
	endTime := pointTime;
	delta := pointValue - startValue;

	temp := sr.NSlopeUnits()
	totalTime := endTime - baseTime;

	nSlopes := len(sr.Slopes)

	newPtVals := make([]float64, nSlopes-1)
	for i := 1; i < nSlopes + 1; i++ {
		temp1 := sr.Slopes[i-1].Slope / temp; /* Calculate normal slope */

		/* Calculate time interval */
		intervalTime := PointTime(sr.Points[i], seq.Model) - PointTime(sr.Points[i-1], seq.Model)

		/* Apply interval percentage to slope */
		temp1 = temp1 * (intervalTime / totalTime)

		/* Multiply by delta and add to last point */
		temp1 = temp1 * delta;
		sum += temp1;

		if i < nSlopes {
			newPtVals[i - 1] = temp1;
		}
	}
	factor := delta / sum;
	temp = startValue;

	value := 0.0;
	pts := len(sr.Points)
	for i := 0; i < pts; i++ {
		pt := sr.Points[i];

		if i >= 1 && i < pts - 1 {
			pointTime = PointTime(pt, seq.Model)

			pointValue = newPtVals[i - 1];
			pointValue *= factor;
			pointValue += temp;
			temp = pointValue;
		} else {
			pointTime, pointValue = PointData(pt, seq.Model)
		}

		value = baseline + ((pointValue / 100.0) * paramDelta)
		if value < min {
			value = min;
		} else if value > max {
			value = max;
		}
		if !pt.IsPhantom {
			seq.InsertEvent(evIdx, pointTime, value)
		}
	}
	return value;
}

// It is assumed that postureList.size() >= 2.
// ApplyRule
func (seq *Sequence) ApplyRule(rule *Rule, postures []Posture, tempos []float64, postureIdx int) {
	var val float64
	ruleSyms := []float64{0.0, 0.0, 0.0, 0.0, 0.0}

	rule.EvalExpr(tempos, postures, seq.Model, ruleSyms)
	seq.Multiplier = 1.0 / seq.PostureDatum[postureIdx].RuleTempo
	phtype := len(rule.BoolExprs)
	seq.Duration = int(ruleSyms[0] * seq.Multiplier)

	seq.RuleDatum[seq.CurRule].FirstPosture = postureIdx
	seq.RuleDatum[seq.CurRule].LastPosture = postureIdx + int(phtype-1)
	seq.RuleDatum[seq.CurRule].Beat = (ruleSyms[1] * seq.Multiplier) + float64(seq.ZeroRef)
	seq.RuleDatum[seq.CurRule].Duration = ruleSyms[0] * seq.Multiplier
	seq.CurRule++
	rd := RuleData{}
	seq.RuleDatum = append(seq.RuleDatum, rd)

	var tempEvent *Event

	switch phtype {
	/* Note: Case 4 should execute all of the below, case 3 the last two */
	case TetraPhone:
		if len(postures) == 4 {
			seq.PostureDatum[postureIdx+3].Onset = float64(seq.ZeroRef) + ruleSyms[1]
			tempEvent = seq.InsertEvent(-1, ruleSyms[3], 0.0)
			if tempEvent != nil {
				tempEvent.Flag = 1
			}
		}
		fallthrough
	case TriPhone:
		if len(postures) >= 3 {
			seq.PostureDatum[postureIdx+2].Onset = float64(seq.ZeroRef) + ruleSyms[1]
			tempEvent = seq.InsertEvent(-1, ruleSyms[2], 0.0)
			if tempEvent != nil {
				tempEvent.Flag = 1
			}
		}
		fallthrough
	case DiPhone:
		seq.PostureDatum[postureIdx+1].Onset = float64(seq.ZeroRef) + ruleSyms[1]
		tempEvent = seq.InsertEvent(-1, 0.0, 0.0)
		if tempEvent != nil {
			tempEvent.Flag = 1
		}
	default:
		log.Println("ApplyRule fell through switch")
	}
	//tempTargets = (List *) [rule parameterList]  // commented out in C++

	var targets [4]float64
	/* Loop through the parameters */
	for i := 0; i < len(seq.Model.Params); i++ {
		/* Get actual parameter target values */
		targets[0] = postures[0].ParamTargets[i]
		targets[1] = postures[1].ParamTargets[i]
		targets[2] = 0.0
		targets[3] = 0.0
		if len(postures) >= 3 {
			targets[2] = postures[2].ParamTargets[i]
		}
		if len(postures) == 4 {
			targets[3] = postures[3].ParamTargets[i]
		}

		/* Optimization, Don't calculate if no changes occur */
		cont := true
		// no fallthrough
		switch phtype {
		case DiPhone:
			if targets[0] == targets[1] {
				cont = false
			}
		case TriPhone:
			if (targets[0] == targets[1]) && (targets[0] == targets[2] {
				cont = false
			}
		case TetraPhone:
			if (targets[0] == targets[1]) && (targets[0] == targets[2]) && (targets[0] == targets[3] {
				cont = false
			}
		default:
			log.Println("ApplyRule fell through switch")
		}

		seq.InsertEvent(i, 0.0, targets[0])

		if cont {
			curType := DiPhone
			curDelta := targets[1] - targets[0]
			lastVal := targets[0]
			//lastValue = 0.0 // commented out in C++

			transition := &rule.ParamProfileTransitions[i]
			if transition == nil {
				log.Println("Rule tranisition not found")
				return
			}

			/* Apply lists to parameter */
			for j := 0; j < len(transition.PtSlpList); j++ {
				pointOrSlope := transition.PtSlpList[j]
				slopeRatio, ok := pointOrSlope.(SlopeRatio)
				if ok { // is SlopeRatio
					if int(slopeRatio.Points[0].TType) != curType { //TODO: check pointList.size() > 0
						curType = int(slopeRatio.Points[0].TType)
						targets[curType-2] = lastVal
						curDelta = targets[curType-1] - lastVal
					}
					v := seq.SlopeRatioEvents(i, &slopeRatio, targets[curType-2], curDelta,
						seq.Min[i], seq.Max[i])
					val = v
				} else {
					pt, ok := pointOrSlope.(Point)
					if ok { // is SlopeRatio
						if int(pt.TType) != curType {
							curType = int(pt.TType)
							targets[curType-2] = lastVal
							curDelta = targets[curType-1] - lastVal
						}
						ptTime, v := PointData(pt, seq.Model)
						val = v
						if !pt.IsPhantom {
							seq.InsertEvent(i, ptTime, val)
						}
					}
					lastVal = val
				}
			}
		}
		//else {
		//	insertEvent(i, 0.0, targets[0])
		//}

	/* Special Event Profiles */
	for i := 0; i < len(seq.Model.Params); i++ {
		spTrans := rule.SpecialProfileTransitions[i]
			for j := 0; j < len(spTrans.PtSlpList); j++ {
				pointOrSlope := spTrans.PtSlpList[j]
				pt, ok := pointOrSlope.(Point)
				if !ok {
					log.Println("Apply Rule: type assertion failure - not a Point")
				}

				/* calculate time of event */
				tempTime := PointTime(pt, seq.Model)

				/* Calculate value of event */
				value := (pt.Value / 100.0) * (seq.Max[i] - seq.Min[i])
				//maxValue = value; // commented out in C++

				/* insert event into event list */
				seq.InsertEvent(i + 16, tempTime, value)
			}
		}
	}

	seq.SetZeroRef(int(ruleSyms[0] * seq.Multiplier) + seq.ZeroRef)
	tempEvent = seq.InsertEvent(-1, 0.0, 0.0)
	if tempEvent != nil {
		tempEvent.Flag = 1
	}
}

func (seq *Sequence) GenerateEventList() {
	for i := 0; i < 16; i++ { //TODO: replace hard-coded value
		param := seq.Model.Params[i]
		seq.Min[i] = param.Min
		seq.Max[i] = param.Max
	}

	/* Calculate Rhythm including regression */
	for i := 0; i < seq.CurFoot; i++ {
		rus := seq.Feet[i].End - seq.Feet[i].Start + 1;
		/* Apply rhythm model */
		var footTempo float64
		var tempTempo float64
		if seq.Feet[i].Marked {
			tempTempo = 117.7 - (19.36 * float64(rus))
			seq.Feet[i].Tempo -= tempTempo / 180.0;
			footTempo = seq.GlobalTempo * seq.Feet[i].Tempo;
		} else {
			tempTempo := 18.5 - (2.08 * float64(rus))
			seq.Feet[i].Tempo -= tempTempo / 140.0;
			footTempo = seq.GlobalTempo * seq.Feet[i].Tempo;
		}
		for j := seq.Feet[i].Start; j < seq.Feet[i].End + 1; j++ {
			seq.PostureTempos[j] *= footTempo;
			if seq.PostureTempos[j] < 0.2 {
				seq.PostureTempos[j] = 0.2;
			} else if seq.PostureTempos[j] > 2.0 {
				seq.PostureTempos[j] = 2.0;
			}
		}
	}

	basePosIdx := 0;
	var tempPostures = []Posture{}
	for basePosIdx < seq.CurPosture {
		tempPostures = tempPostures[:0]
		for i := 0; i < 4; i++ {
			postureIndex := basePosIdx + i;
			if postureIndex <= seq.CurPosture && seq.PostureDatum[postureIndex].Posture != nil  {
				tempPostures = append(tempPostures, *(seq.PostureDatum[postureIndex].Posture))
			} else {
				break;
			}
		}
		if len(tempPostures) < 2 {
			break;
		}
		ruleIndex := 0;
		tempRule, ruleIndex := seq.Model.FirstRule(tempPostures)
		if tempRule == nil {
			log.Println("Could not find a matching rule.")
			return
		}

		seq.RuleDatum[seq.CurRule].Number = ruleIndex + 1
		seq.ApplyRule(tempRule, tempPostures, seq.PostureTempos, basePosIdx)
		basePosIdx += len(tempRule.BoolExprs) - 1;
	}

	//[dataPtr[numElements-1] setFlag:1];
}

func (seq *Sequence) SetFullTimeScale() {
	seq.ZeroRef = 0;
	seq.ZeroIdx = 0;
	seq.Duration = seq.Events[len(seq.Events)-1].Time + 100;
}

// IntRange is the way to specify the min and max values for an integer range (used here when generating random numbers)
type IntRange struct {
	Min int
	Max int
}

func (seq *Sequence) ApplyIntonation() {
	var tgRandom int
	ruleIndex := 0
	//double pretonicDelta, offsetTime = 0.0;
	//double randomSemitone, randomSlope;

	seq.ZeroRef = 0;
	seq.ZeroIdx = 0;
	seq.Duration = seq.Events[len(seq.Events) - 1].Time + 100
	seq.IntonationPts = seq.IntonationPts[:0]  // clear

	vocoidCategory := seq.Model.CategoryTry("vocoid")
	if vocoidCategory == nil {
		log.Println("Could not find the category \"vocoid\".")
		return
	}

	var randDist0 IntRange
	var randDist1 IntRange
	var randDist2 IntRange
	var randDist3 IntRange

	if seq.TgCount[0] > 0 {
		randDist0.Max = seq.TgCount[0]-1
	}
	if seq.TgCount[1] > 0 {
		randDist1.Max = seq.TgCount[1]-1
	}
	if seq.TgCount[2] > 0 {
		randDist2.Max = seq.TgCount[2]-1
	}
	if seq.TgCount[3] > 0 {
		randDist3.Max = seq.TgCount[3]-1
	}

	for i := 0; i < seq.CurToneGroup; i++ {
		firstFoot := seq.ToneGroups[i].StartFoot
		endFoot := seq.ToneGroups[i].EndFoot

		startTime := seq.PostureDatum[seq.Feet[firstFoot].Start].Onset
		endTime := seq.PostureDatum[seq.Feet[endFoot].End].Onset

		//printf("Tg: %d First: %d  end: %d  StartTime: %f  endTime: %f\n", i, firstFoot, endFoot, startTime, endTime)

		if seq.UseFixedIntonationParams {
			seq.IntonationParams = seq.FixedIntonationParams
		} else {
			switch seq.ToneGroups[i].Type {
			default:
			case ToneStatement:
				if seq.TgUseRandom {
					tgRandom = rand.Intn(randDist0.Max)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = seq.TgParams[0][tgRandom * 10]
			case ToneExclamation:
				if seq.TgUseRandom {
					tgRandom = rand.Intn(randDist0.Max)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = seq.TgParams[0][tgRandom * 10];
			case ToneQuestion:
				if seq.TgUseRandom {
					tgRandom = rand.Intn(randDist1.Max)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = seq.TgParams[1][tgRandom * 10];
			case ToneContinuation:
				if seq.TgUseRandom {
					tgRandom = rand.Intn(randDist2.Max)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = seq.TgParams[2][tgRandom * 10];
			case ToneSemicolon:
				if seq.TgUseRandom {
					tgRandom = rand.Intn(randDist3.Max)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = seq.TgParams[3][tgRandom * 10];
			}
		}

		//printf("Intonation Parameters: Type : %d  random: %d\n", toneGroups[i].type, tgRandom)
		//for (j = 0; j<6; j++
		//	printf("%f ", intonParms[j])
		//printf("\n")

		pretonicDelta := 0.0
		deltaTime := endTime - startTime;
		if deltaTime < eps {
			pretonicDelta = 0
		} else {
			pretonicDelta = seq.IntonationParams[1] / deltaTime
		}
		//printf("Pretonic Delta = %f time = %f\n", pretonicDelta, (endTime - startTime))

		/* Set up intonation boundary variables */
		for j := firstFoot; j <= endFoot; j++ {
			postureIndex := seq.Feet[j].Start
			while (!seq.PostureDatum[postureIndex].posture->isMemberOfCategory(*vocoidCategory) {
				postureIndex++;
				//printf("Checking posture %s for vocoid\n", [posture[postureIndex].posture symbol])
				if postureIndex > seq.Feet[j].End {
					postureIndex = seq.Feet[j].Start;
					break;
				}
			}

			if !seq.Feet[j].Marked {
				for k := 0; k < seq.CurRule; k++ {
					if (postureIndex >= seq.RuleDatum[k].firstPosture) && (postureIndex <= seq.RuleDatum[k].lastPosture) {
						ruleIndex = k;
						break;
					}
				}

				if seq.TgUseRandom {
					randomSemitone = randDist_(randSrc_) * seq.IntonationParams[3] - seq.IntonationParams[3] / 2.0;
					randomSlope = randDist_(randSrc_) * 0.015 + 0.01;
				} else {
					randomSemitone = 0.0;
					randomSlope = 0.02;
				}

				//printf("postureIndex = %d onsetTime : %f Delta: %f\n", postureIndex,
				//	postures[postureIndex].onset-startTime,
				//	((postures[postureIndex].onset-startTime)*pretonicDelta) + intonParms[1] + randomSemitone)

				addIntonationPoint((seq.PostureDatum[postureIndex].onset - startTime) * pretonicDelta + seq.IntonationParams[1] + randomSemitone,
							offsetTime, randomSlope, ruleIndex)
			} else { /* Tonic */
				if seq.ToneGroups[i].type == 3 {
					randomSlope = 0.01;
				} else {
					randomSlope = 0.02;
				}

				for (k = 0; k < seq.CurRule; k++ {
					if (postureIndex >= seq.RuleDatum[k].firstPosture) && (postureIndex <= seq.RuleDatum[k].lastPosture) {
						ruleIndex = k;
						break;
					}
				}

				if seq.TgUseRandom {
					randomSemitone = randDist_(randSrc_) * seq.IntonationParams[6] - seq.IntonationParams[6] / 2.0;
					randomSlope += randDist_(randSrc_) * 0.03;
				} else {
					randomSemitone = 0.0;
					randomSlope += 0.03;
				}
				addIntonationPoint(seq.IntonationParams[2] + seq.IntonationParams[1] + randomSemitone,
							offsetTime, randomSlope, ruleIndex)

				postureIndex = seq.Feet[j].end;
				for (k = ruleIndex; k < seq.CurRule; k++ {
					if (postureIndex >= seq.RuleDatum[k].firstPosture) && (postureIndex <= seq.RuleDatum[k].lastPosture) {
						ruleIndex = k;
						break;
					}
				}

				addIntonationPoint(seq.IntonationParams[2] + seq.IntonationParams[1] + seq.IntonationParams[5],
							0.0, 0.0, ruleIndex)
			}
			offsetTime = -40.0;
		}
	}
	addIntonationPoint(seq.IntonationParams[2] + seq.IntonationParams[1] + seq.IntonationParams[5],
				0.0, 0.0, seq.CurRule - 1)

}

func (seq *Sequence) ApplyIntonationSmooth() {
	seq.SetFullTimeScale()
	//tempPoint = [[IntonationPoint alloc] initWithEventList: self]; //commented out in C++ version
	//[tempPoint setSemitone: -20.0]; //commented out in C++ version
	//[tempPoint setSemitone: -20.0]; //commented out in C++ version
	//[tempPoint setRuleIndex: 0]; //commented out in C++ version
	//[tempPoint setOffsetTime: 10.0 - [self getBeatAtIndex:(int) 0]]; //commented out in C++ version
	//[intonationPoints insertObject: tempPoint at:0]; //commented out in C++ version

	for j := 0; j < len(seq.IntonationPts) - 1; j++ {
		point1 := seq.IntonationPts[j];
		point2 := seq.IntonationPts[j + 1];

		point1.AbsTime()
		x1 := point1.AbsTime() / 4.0;
		y1 := point1.SemiTone + 20.0;
		m1 := point1.Slope

		x2 := point2.AbsTime() / 4.0;
		y2 := point2.SemiTone + 20.0;
		m2 := point2.Slope

		x12 := x1 * x1;
		x13 := x12 * x1;

		x22 := x2 * x2;
		x23 := x22 * x2;

		denominator := x2 - x1;
		denominator = denominator * denominator * denominator;

//		double d = ( -(y2 * x13) + 3 * y2 * x12 * x2 + m2 * x13 * x2 + m1 * x12 * x22 - m2 * x12 * x22 - 3 * x1 * y1 * x22 - m1 * x1 * x23 + y1 * x23 ) / denominator; // commmented out in C++
		c := ( -(m2 * x13) - 6 * y2 * x1 * x2 - 2 * m1 * x12 * x2 - m2 * x12 * x2 + 6 * x1 * y1 * x2 + m1 * x1 * x22 + 2 * m2 * x1 * x22 + m1 * x23 ) / denominator
		b := ( 3 * y2 * x1 + m1 * x12 + 2 * m2 * x12 - 3 * x1 * y1 + 3 * x2 * y2 + m1 * x1 * x2 - m2 * x1 * x2 - 3 * y1 * x2 - 2 * m1 * x22 - m2 * x22 / denominator
		a := ( -2 * y2 - m1 * x1 - m2 * x1 + 2 * y1 + m1 * x2 + m2 * x2) / denominator

		seq.InsertEvent(32, point1.AbsTime(), point1.SemiTone)
		//printf("Inserting Point %f\n", [point1 semitone])
		yTemp := (3.0 * a * x12) + (2.0 * b * x1) + c;
		seq.InsertEvent(33, point1.AbsTime(), yTemp)
		yTemp = (6.0 * a * x1) + (2.0 * b)
		seq.InsertEvent(34, point1.AbsTime(), yTemp)
		yTemp = 6.0 * a;
		seq.InsertEvent(35, point1.AbsTime(), yTemp)
	}
	//[intonationPoints removeObjectAt:0]; //commented out in C++ version

	//[self insertEvent:32 atTime: 0.0 withValue: -20.0]; /* A value of -20.0 in bin 32 should produce a linear interp to -20.0 */ //commented out in C++ version
}

func (seq *Sequence) AddIntonationPoint(semiTone, offsetTime, slope float64, ruleIdx int) {

	if ruleIdx > seq.CurRule {
		return;
	}

	iPt := IntonationPt{}
	iPt.RuleIdx = ruleIdx
	iPt.Offset = offsetTime
	iPt.SemiTone = semiTone
	iPt.Slope = slope

	time := iPt.AbsTime()
	for i := 0; i < len(seq.IntonationPts); i++ {
		if time < seq.IntonationPts[i].AbsTime() {
			seq.IntonationPts.insert(seq.IntonationPts.begin() + i, iPoint)
			return;
		}
	}
	seq.IntonationPts = append(seq.IntonationPts, iPt)
}

void
EventList::generateOutput(std::ostream& trmParamStream)
{
	double currentValues[36];
	double currentDeltas[36];
	double temp;
	float table[16];

	if len(seq.Events) == 0 {
		return;
	}

	for i := 0; i < 16; i++ {
		int j = 1;
		while ((temp = seq.Events[j]->getValue(i)) == GS_EVENTeventsINVALID_EVENT_VALUE {
			j++;
			if j >= len(seq.Events)) break;
		}
		currentValues[i] = seq.Events[0]->getValue(i)
		if j < len(seq.Events) {
			currentDeltas[i] = ((temp - currentValues[i]) / (double) (seq.Events[j]->time)) * 4.0;
		} else {
			currentDeltas[i] = 0.0;
		}
	}
	for int i = 16; i < 36; i++ {
		currentValues[i] = currentDeltas[i] = 0.0;
	}

	if smoothIntonation_ {
		j := 0;
		while ((temp = seq.Events[j]->getValue(32)) == GS_EVENTeventsINVALID_EVENT_VALUE {
			j++;
			if j >= len(seq.Events)) break;
		}
		if j < len(seq.Events) {
			currentValues[32] = seq.Events[j]->getValue(32)
		} else {
			currentValues[32] = 0.0;
		}
		currentDeltas[32] = 0.0;
	} else {
		int j = 1;
		while ((temp = seq.Events[j]->getValue(32)) == GS_EVENTeventsINVALID_EVENT_VALUE {
			j++;
			if j >= len(seq.Events)) break;
		}
		currentValues[32] = seq.Events[0]->getValue(32)
		if j < len(seq.Events) {
			currentDeltas[32] = ((temp - currentValues[32]) / (double) (seq.Events[j]->time)) * 4.0;
		} else {
			currentDeltas[32] = 0.0;
		}
		currentValues[32] = -20.0;
	}

	int index = 1;
	int currentTime = 0;
	int nextTime = seq.Events[1]->time;
	while (index < len(seq.Events) {

		for (j := 0; j < 16; j++ {
			table[j] = (float) currentValues[j] + (float) currentValues[j + 16];
		}
		if !seq.MicroFlag_) table[0] = 0.0;
		if driftFlag_)  table[0] += static_cast<float>(driftGenerator_.drift())
		if seq.MacroFlag)  table[0] += static_cast<float>(currentValues[32])

		table[0] += static_cast<float>(seq.PitchMean)

		//trmParamStream << std::fixed << std::setprecision(3)
		trmParamStream << table[0];
		for int k = 1; k < 7; ++k {
			trmParamStream << ' ' << table[k];
		}
		for int k = 7; k < 15; ++k { // R1 - R8
			trmParamStream << ' ' << table[k] * radiusCoef[k - 7];
		}
		trmParamStream << ' ' << table[15];
		trmParamStream << '\n';

		for (j := 0; j < 32; j++ {
			if currentDeltas[j] {
				currentValues[j] += currentDeltas[j];
			}
		}

		if smoothIntonation_ {
			currentDeltas[34] += currentDeltas[35];
			currentDeltas[33] += currentDeltas[34];
			currentValues[32] += currentDeltas[33];
		} else {
			if currentDeltas[32] {
				currentValues[32] += currentDeltas[32];
			}
		}
		currentTime += 4;

		if currentTime >= nextTime {
			++index;
			if index == len(seq.Events) {
				break;
			}
			nextTime = seq.Events[index]->time;
			for (j := 0; j < 33; j++ { /* 32? 33? */
				if seq.Events[index - 1]->getValue(j) != GS_EVENTeventsINVALID_EVENT_VALUE {
					int k = index;
					while ((temp = seq.Events[k]->getValue(j)) == GS_EVENTeventsINVALID_EVENT_VALUE {
						if k >= len(seq.Events) - 1U {
							currentDeltas[j] = 0.0;
							break;
						}
						k++;
					}
					if temp != GS_EVENTeventsINVALID_EVENT_VALUE {
						currentDeltas[j] = (temp - currentValues[j]) /
									(double) (seq.Events[k]->time - currentTime) * 4.0;
					}
				}
			}
			if smoothIntonation_ {
				if seq.Events[index - 1]->getValue(33) != GS_EVENTeventsINVALID_EVENT_VALUE {
					currentValues[32] = seq.Events[index - 1]->getValue(32)
					currentDeltas[32] = 0.0;
					currentDeltas[33] = seq.Events[index - 1]->getValue(33)
					currentDeltas[34] = seq.Events[index - 1]->getValue(34)
					currentDeltas[35] = seq.Events[index - 1]->getValue(35)
				}
			}
		}
	}

	if Log::debugEnabled {
		printDataStructures()
	}
}

void
EventList::clearMacroIntonation()
{
	for i := 0, size = len(seq.Events); i < size; ++i {
		auto& event = seq.Events[i];
		for j :=32; j < 36; ++j {
			event->setValue(GS_EVENTeventsINVALID_EVENT_VALUE, j)
		}
	}
}

void
EventList::printDataStructures()
{
	printf("Tone Groups %d\n", currentToneGroup_)
	for i := 0; i < currentToneGroup_; i++ {
		printf("%d  start: %d  end: %d  type: %d\n", i, seq.ToneGroups[i].startFoot, seq.ToneGroups[i].endFoot,
			seq.ToneGroups[i].type)
	}

	printf("\nFeet %d\n", currentFoot_)
	for i := 0; i < currentFoot_; i++ {
		printf("%d  tempo: %f start: %d  end: %d  marked: %d last: %d onset1: %f onset2: %f\n", i, seq.Feet[i].tempo,
			seq.Feet[i].start, seq.Feet[i].end, seq.Feet[i].marked, seq.Feet[i].last, seq.Feet[i].onset1, seq.Feet[i].onset2)
	}

	printf("\nPostures %d\n", seq.CurPosture)
	for i := 0; i < seq.CurPosture; i++ {
		printf("%u  \"%s\" tempo: %f syllable: %d onset: %f ruleTempo: %f\n",
			 i, seq.PostureDatum[i].posture->name().c_str(), seq.PostureTempos[i], seq.PostureDatum[i].syllable, seq.PostureDatum[i].onset, seq.PostureDatum[i].ruleTempo)
	}

	printf("\nRules %d\n", seq.CurRule)
	for i := 0; i < seq.CurRule; i++ {
		printf("Number: %d  start: %d  end: %d  duration %f\n", seq.RuleDatum[i].number, seq.RuleDatum[i].firstPosture,
			seq.RuleDatum[i].lastPosture, seq.RuleDatum[i].duration)
	}
#if 0
	printf("\nEvents %lu\n", len(seq.Events))
	for i := 0; i < len(seq.Events); i++ {
		const Event& event = *seq.Events[i];
		printf("  Event: time=%d flag=%d\n    Values: ", event.time, event.flag)

		for (j := 0; j < 16; j++ {
			printf("%.3f ", event.getValue(j))
		}
		printf("\n            ")
		for j :=16; j < 32; j++ {
			printf("%.3f ", event.getValue(j))
		}
		printf("\n            ")
		for j :=32; j < Event::EVENTS_SIZE; j++ {
			printf("%.3f ", event.getValue(j))
		}
		printf("\n")
	}
#endif
}
