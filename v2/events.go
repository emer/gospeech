// Copyright (c) 2019, The Emergent Authors. All rights reserved.
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
	"math"
	"os"
)

const diphone = 2
const triphone = 3
const tetraphone = 4

const intonationConfigFileName = "/intonation"
const eps = 1.0e-6

// const toneStatement = 0
// const toneExclamation = 1
// const toneQuestion = 2
// const toneContinuation = 3
// const toneSemicolon = 4

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
	Marked int
	Last   int
}

func (ft *Foot) Defaults( {
	ft.Onset1 = 0.0
	ft.Onset2 = 0.0
	ft.Tempo = 1.0
	ft.Start = 0
	ft.End = 0
	ft.Marked = 0
	ft.Last = 0
}

type ToneGroup struct {
	StartFoot int
	EndFoot   int
	Type      int
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

	ZeroRef             int
	ZeroIdx             int
	Duration            int
	TimeQuant           int
	MacroFlag           int
	MicroFlag           int
	DriftFlag           int
	SmoothInton         int
	PitchMean           float64
	GlobalTempo         float64
	Multiplier          float64
	IntonationParams    []float64
	PostureDatum        []PostureData
	PostureTempos       []float64
	CurPosture          int
	Feet                []Foot
	CurFoot             int
	ToneGroups          []ToneGroup
	CurToneGroup        int
	RuleDatum           []RuleData
	CurRule             int
	Min                 [16]float64
	Max                 [16]float64
	IntonationPts       []IntonationPt
	Drift               Drift
	TgUseRandom         bool
	IntonRandom         float64
	TgParams            []float64
	TgCount             [5]int
	UseFixedIntonationParams bool
	FixedIntonationParams    [10]float64
	RadiusCoef          [8]float64 // TRM::Tube::TOTAL_REGIONS

	// std::random_device randDev_;
	// std::mt19937 randSrc_;
	// std::uniform_real_distribution<> randDist_;

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
	seq.Model = model
	seq.Defaults()
	seq.TgParams = make([]float64, 5)
	seq.InitToneGroups(config)

	for i := 0; i < 10; i++ {
		seq.FixedIntonationParams[i] = 0.0
	}
	for i := 0; i < 8; i++ { // TRM::Tube::TOTAL_REGIONS
		seq.RadiusCoef[i] = 1.0
	}

	Events.reserve(128)

	for i := 0; i < 10; i++ {
		seq.FixedIntonationParams[i] = 0.0
	}
	for i := 0; i < 8; i++ { //TRM::Tube::TOTAL_REGIONS
		seq.RadiusCoef[i] = 1.0
	}

}

func (seq *Sequence) SetIntonationParams(pitch, pretonicRange, pretonicLift, tonicRange, tonicMovement float64 {
	seq.FixedIntonationParams[1] = pitch
	seq.FixedIntonationParams[2] = pretonicRange
	seq.FixedIntonationParams[3] = pretonicLift
	seq.FixedIntonationParams[5] = tonicRange
	seq.FixedIntonationParams[6] = tonicMovement
}

func (seq *Sequence) SetRadiusCoefs(values []float64 {
	for i := 0; i < 8; i++ { // TRM::Tube::TOTAL_REGIONS
		seq.RadiusCoef[i] = values[i]
	}
}

func (seq *Sequence) ParseGroups(idx int, number int, fp *os.File) {
	char line[256];
	seq.TgParams[idx].resize(10 * number)
	for i := 0; i < number; i++ {
		fgets(line, 256, fp)
		float* temp = &seq.TgParams[index][i * 10];
		sscanf(line, " %f %f %f %f %f %f %f %f %f %f",
			&temp[0], &temp[1], &temp[2], &temp[3], &temp[4],
			&temp[5], &temp[6], &temp[7], &temp[8], &temp[9])
	}
}

func (seq *Sequence) InitToneGroups(configDirPath string {
	var fp *os.File
	var line [256]byte
	count := 0
	
	std::ostringstream path;
	path << configDirPath << INTONATION_CONFIG_FILE_NAME;
	fp = os.Open(path.str().c_str(), "rb")
	if fp == nil {
		THROW_EXCEPTION(IOException, "Could not open the file " << path.str().c_str() << '.')
	}
	while (fgets(line, 256, fp) != NULL) {
		if (line[0] == '#') || (line[0] == ' ') {
			// Skip.
		} else if strncmp(line, "TG", 2) == 0 {
			sscanf(&line[2], " %d", &tgCount_[count])
			parseGroups(count, tgCount_[count], fp)
			count++;
		} else if strncmp(line, "RANDOM", 6) == 0 {
			sscanf(&line[6], " %f", &intonationRandom_)
		}
	}
	fp.Close()
	
	if Log::debugEnabled {
		printToneGroups()
	}
}

func (seq *Sequence) PrintToneGroups( {
	// printf("===== Intonation configuration:\n")
	// printf("Intonation random = %f\n", intonationRandom_)
	// printf("Tone groups: %d %d %d %d %d\n", tgCount_[0], tgCount_[1], tgCount_[2], tgCount_[3], tgCount_[4])
	//
	// for (int i = 0; i < 5; i++ {
	// 	float* temp = &seq.TgParams[i][0];
	// 	printf("Temp [%d] = %p\n", i, temp)
	// 	int j = 0;
	// 	for (int k = 0; k < tgCount_[i]; k++ {
	// 		printf("%f %f %f %f %f %f %f %f %f %f\n",
	// 			temp[j]  , temp[j+1], temp[j+2], temp[j+3], temp[j+4],
	// 			temp[j+5], temp[j+6], temp[j+7], temp[j+8], temp[j+9])
	// 		j += 10;
	// 	}
	// }
}

func (seq *Sequence) GetBeatAtIndex(ruleIdx int) float64 {
	if ruleIdx > seq.CurRule {
		return 0.0
	} else {
		// return ruleData_[ruleIndex].beat;
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

// void
// EventList::setCurrentFootMarked()
// {
// 	seq.Feet[currentFoot_].marked = 1;
// }
//
// void
// EventList::setCurrentFootLast()
// {
// 	seq.Feet[currentFoot_].last = 1;
// }
//
// void
// EventList::setCurrentFootTempo(double tempo)
// {
// 	seq.Feet[currentFoot_].tempo = tempo;
// }
//
// void
// EventList::setCurrentPostureTempo(double tempo)
// {
// 	seq.PostureTempos[seq.CurPosture] = tempo;
// }
//
// void
// EventList::setCurrentPostureRuleTempo(float tempo)
// {
// 	seq.PostureDatum[seq.CurPosture].ruleTempo = tempo;
// }

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

func (seq *Sequence) SlopeRatioEvents(evIdx int, slopeRatio SlopeRatio, baseline, paramDelta, min, max float64) {
	sum := 0.0
	var pointTime float64
	var pointValue float64

	Transition.GetPointData(*slopeRatio.pointList.front(), seq.Model, &pointTime, &pointValue)
	baseTime := pointTime;
	startValue := pointValue;

	Transition.GetPointData(*slopeRatio.pointList.back(), seq.Model, pointTime, pointValue)
	endTime := pointTime;
	delta := pointValue - startValue;

	temp := slopeRatio.totalSlopeUnits()
	totalTime := endTime - baseTime;

	int numSlopes = slopeRatio.slopeList.size()
	std::vector<double> newPointValues(numSlopes - 1)
	for int i := 1; i < numSlopes + 1; i++ {
		temp1 := slopeRatio.slopeList[i - 1]->slope / temp; /* Calculate normal slope */

		/* Calculate time interval */
		intervalTime := Transition::getPointTime(*slopeRatio.pointList[i], seq.Model)
				- Transition::getPointTime(*slopeRatio.pointList[i - 1], seq.Model)

		/* Apply interval percentage to slope */
		temp1 = temp1 * (intervalTime / totalTime)

		/* Multiply by delta and add to last point */
		temp1 = temp1 * delta;
		sum += temp1;

		if i < numSlopes {
			newPointValues[i - 1] = temp1;
		}
	}
	factor := delta / sum;
	temp = startValue;

	double value = 0.0;
	for i := 0, size = slopeRatio.pointList.size(); i < size; i++ {
		const Transition::Point& point = *slopeRatio.pointList[i];

		if i >= 1 && i < slopeRatio.pointList.size(); - 1 {
			pointTime = Transition::getPointTime(point, seq.Model)

			pointValue = newPointValues[i - 1];
			pointValue *= factor;
			pointValue += temp;
			temp = pointValue;
		} else {
			Transition::getPointData(point, seq.Model, pointTime, pointValue)
		}

		value = baseline + ((pointValue / 100.0) * parameterDelta)
		if value < min {
			value = min;
		} else if value > max {
			value = max;
		}
		if !point.isPhantom {
			insertEvent(eventIndex, pointTime, value)
		}
	}

	return value;
}

// It is assumed that postureList.size() >= 2.
// ApplyRule
func (seq *Sequence) ApplyRule(rule *Rule, postures []Posture, tempos float64, postureIdx int) {
	double currentValueDelta, value, lastValue
	double ruleSymbols[5] = {0.0, 0.0, 0.0, 0.0, 0.0}
	double tempTime
	double targets[4]
	Event* tempEvent = nil

	rule.evaluateExpressionSymbols(tempos, postureList, seq.Model, ruleSymbols)

	seq.Multiplier = 1.0 / (double) (seq.PostureDatum[postureIndex].ruleTempo)

	int type = rule.numberOfExpressions()
	setDuration((int) (ruleSymbols[0] * seq.Multiplier))

	seq.RuleDatum[seq.CurRule].firstPosture = postureIndex
	seq.RuleDatum[seq.CurRule].lastPosture = postureIndex + (type - 1)
	seq.RuleDatum[seq.CurRule].beat = (ruleSymbols[1] * seq.Multiplier) + (double) seq.ZeroRef
	seq.RuleDatum[seq.CurRule++].duration = ruleSymbols[0] * seq.Multiplier
	seq.RuleDatum.push_back(RuleData())

	switch (type {
	/* Note: Case 4 should execute all of the below, case 3 the last two */
	case 4:
		if postureList.size() == 4 {
			seq.PostureDatum[postureIndex + 3].onset = (double) seq.ZeroRef + ruleSymbols[1]
			tempEvent = insertEvent(-1, ruleSymbols[3], 0.0)
			if tempEvent) tempEvent->flag = 1
		}
	case 3:
		if postureList.size() >= 3 {
			seq.PostureDatum[postureIndex + 2].onset = (double) seq.ZeroRef + ruleSymbols[1]
			tempEvent = insertEvent(-1, ruleSymbols[2], 0.0)
			if tempEvent) tempEvent->flag = 1
		}
	case 2:
		seq.PostureDatum[postureIndex + 1].onset = (double) seq.ZeroRef + ruleSymbols[1]
		tempEvent = insertEvent(-1, 0.0, 0.0)
		if tempEvent) tempEvent->flag = 1
		break
	}

	//tempTargets = (List *) [rule parameterList]

	/* Loop through the parameters */
	for (int i = 0, size = seq.Model.parameterList().size(); i < size; i++) {
		/* Get actual parameter target values */
		targets[0] = postureList[0]->getParameterTarget(i)
		targets[1] = postureList[1]->getParameterTarget(i)
		targets[2] = (postureList.size() >= 3) ? postureList[2]->getParameterTarget(i) : 0.0
		targets[3] = (postureList.size() == 4) ? postureList[3]->getParameterTarget(i) : 0.0

		/* Optimization, Don't calculate if no changes occur */
		cont := 1
		switch (type {
		case DIPHONE:
			if targets[0] == targets[1] {
				cont = 0
			}
			break
		case TRIPHONE:
			if (targets[0] == targets[1]) && (targets[0] == targets[2] {
				cont = 0
			}
			break
		case TETRAPHONE:
			if (targets[0] == targets[1]) && (targets[0] == targets[2]) && (targets[0] == targets[3] {
				cont = 0
			}
			break
		}

		insertEvent(i, 0.0, targets[0])

		if cont {
			curType := DIPHONE
			currentValueDelta = targets[1] - targets[0]
			lastValue = targets[0]
			//lastValue = 0.0

			const std::shared_ptr<Transition> transition = rule.getParamProfileTransition(i)
			if !transition {
				THROW_EXCEPTION(UnavailableResourceException, "Rule transition not found: " << i << '.')
			}

			/* Apply lists to parameter */
			for (j := 0; j < transition->pointOrSlopeList().size(); ++j) {
				const Transition::PointOrSlope& pointOrSlope = *transition->pointOrSlopeList()[j]
				if pointOrSlope.isSlopeRatio() {
					const auto& slopeRatio = dynamic_cast<const Transition::SlopeRatio&>(pointOrSlope)

					if slopeRatio.pointList[0]->type != currentType { //TODO: check pointList.size() > 0
						currentType = slopeRatio.pointList[0]->type
						targets[currentType - 2] = lastValue
						currentValueDelta = targets[currentType - 1] - lastValue
					}
					value = createSlopeRatioEvents(
							slopeRatio, targets[currentType - 2], currentValueDelta,
							min_[i], max_[i], i)
				} else {
					const auto& point = dynamic_cast<const Transition::Point&>(pointOrSlope)

					if point.type != currentType {
						currentType = point.type
						targets[currentType - 2] = lastValue
						currentValueDelta = targets[currentType - 1] - lastValue
					}
					double pointTime
					Transition::getPointData(point, seq.Model,
									targets[currentType - 2], currentValueDelta, min_[i], max_[i],
									pointTime, value)
					if !point.isPhantom {
						insertEvent(i, pointTime, value)
					}
				}
				lastValue = value
			}
		}
		//else {
		//	insertEvent(i, 0.0, targets[0])
		//}
	}

	/* Special Event Profiles */
	for (int i = 0, size = seq.Model.parameterList().size(); i < size; i++ {
		const std::shared_ptr<Transition> specialTransition = rule.getSpecialProfileTransition(i)
		if specialTransition {
			for (j := 0; j < specialTransition->pointOrSlopeList().size(); ++j {
				const Transition::PointOrSlope& pointOrSlope = *specialTransition->pointOrSlopeList()[j];
				const auto& point = dynamic_cast<const Transition::Point&>(pointOrSlope)

				/* calculate time of event */
				tempTime = Transition::getPointTime(point, seq.Model)

				/* Calculate value of event */
				value = ((point.value / 100.0) * (max_[i] - min_[i]))
				//maxValue = value;

				/* insert event into event list */
				insertEvent(i + 16U, tempTime, value)
			}
		}
	}

	setZeroRef((int) (ruleSymbols[0] * seq.Multiplier) + seq.ZeroRef)
	tempEvent = insertEvent(-1, 0.0, 0.0)
	if tempEvent) tempEvent->flag = 1;

}

func (seq *Sequence) GenerateEventList( {
	for i := 0; i < 16; i++ { //TODO: replace hard-coded value
		param := seq.Model.GetParameter(i)
		seq.Min[i] = param.minimum()
		seq.Max[i] = param.maximum()
	}

	/* Calculate Rhythm including regression */
	for i := 0; i < seq.CurFoot; i++ {
		rus := seq.Feet[i].End - seq.Feet[i].Start + 1;
		/* Apply rhythm model */
		var footTempo float64
		var tempTempo float64
		if seq.Feet[i].marked {
			tempTempo = 117.7 - (19.36 * float64(rus))
			seq.Feet[i].Tempo -= tempTempo / 180.0;
			footTempo = seq.GlobalTemp * seq.Feet[i].tempo;
		} else {
			tempTempo := 18.5 - (2.08 * float64(rus))
			seq.Feet[i].Tempo -= tempTempo / 140.0;
			footTempo = seq.GlobalTemp * seq.Feet[i].Tempo;
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

	int basePostureIndex = 0;
	std::vector<const Posture*> tempPostureList;
	while (basePostureIndex < seq.CurPosture {
		tempPostureList.clear()
		for i := 0; i < 4; i++ {
			int postureIndex = basePostureIndex + i;
			if postureIndex <= seq.CurPosture && seq.PostureDatum[postureIndex].posture {
				tempPostureList.push_back(seq.PostureDatum[postureIndex].posture)
			} else {
				break;
			}
		}
		if tempPostureList.size() < 2 {
			break;
		}
		int ruleIndex = 0;
		const Rule* tempRule = seq.Model.findFirstMatchingRule(tempPostureList, ruleIndex)
		if tempRule == nullptr {
			THROW_EXCEPTION(UnavailableResourceException, "Could not find a matching rule.")
		}

		seq.RuleDatum[seq.CurRule].number = ruleIndex + 1U;

		applyRule(*tempRule, tempPostureList, &seq.PostureTempos[basePostureIndex], basePostureIndex)

		basePostureIndex += tempRule->numberOfExpressions() - 1;
	}

	//[dataPtr[numElements-1] setFlag:1];
}

func (seq *Sequence) SetFullTimeScale( {
	seq.ZeroRef = 0;
	seq.ZeroIdx = 0;
	seq.Duration = Events.back()->time + 100;
}


func (seq *Sequence) ApplyIntonation( {
	var tgRandom int
	ruleIndex := 0
	 postureIndex;
	int i, j, k;
	double pretonicDelta, offsetTime = 0.0;
	double randomSemitone, randomSlope;

	seq.ZeroRef = 0;
	seq.ZeroIdx = 0;
	seq.Duration = Events.back()->time + 100;

	seq.IntonationPts.clear()

	std::shared_ptr<const Category> vocoidCategory = seq.Model.findCategory("vocoid")
	if !vocoidCategory {
		THROW_EXCEPTION(UnavailableResourceException, "Could not find the category \"vocoid\".")
	}

	std::uniform_int_distribution<> intRandDist0(0, tgCount_[0] > 0 ? tgCount_[0] - 1 : 0)
	std::uniform_int_distribution<> intRandDist1(0, tgCount_[1] > 0 ? tgCount_[1] - 1 : 0)
	std::uniform_int_distribution<> intRandDist2(0, tgCount_[2] > 0 ? tgCount_[2] - 1 : 0)
	std::uniform_int_distribution<> intRandDist3(0, tgCount_[3] > 0 ? tgCount_[3] - 1 : 0)

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
			case TONE_GROUP_TYPE_STATEMENT:
				if seq.TgUseRandom {
					tgRandom = intRandDist0(randSrc_)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = &seq.TgParams[0][tgRandom * 10];
				break;
			case TONE_GROUP_TYPE_EXCLAMATION:
				if seq.TgUseRandom {
					tgRandom = intRandDist0(randSrc_)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = &seq.TgParams[0][tgRandom * 10];
				break;
			case TONE_GROUP_TYPE_QUESTION:
				if seq.TgUseRandom {
					tgRandom = intRandDist1(randSrc_)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = &seq.TgParams[1][tgRandom * 10];
				break;
			case TONE_GROUP_TYPE_CONTINUATION:
				if seq.TgUseRandom {
					tgRandom = intRandDist2(randSrc_)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = &seq.TgParams[2][tgRandom * 10];
				break;
			case TONE_GROUP_TYPE_SEMICOLON:
				if seq.TgUseRandom {
					tgRandom = intRandDist3(randSrc_)
				} else {
					tgRandom = 0;
				}
				seq.IntonationParams = &seq.TgParams[3][tgRandom * 10];
				break;
			}
		}

		//printf("Intonation Parameters: Type : %d  random: %d\n", toneGroups[i].type, tgRandom)
		//for (j = 0; j<6; j++
		//	printf("%f ", intonParms[j])
		//printf("\n")

		deltaTime := endTime - startTime;
		pretonicDelta = deltaTime < EPS ? 0.0 : seq.IntonationParams[1] / deltaTime;
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

func (seq *Sequence) ApplyIntonationSmooth( {
	seq.SetFullTimeScale()
	//tempPoint = [[IntonationPoint alloc] initWithEventList: self];
	//[tempPoint setSemitone: -20.0];
	//[tempPoint setSemitone: -20.0];
	//[tempPoint setRuleIndex: 0];
	//[tempPoint setOffsetTime: 10.0 - [self getBeatAtIndex:(int) 0]];

	//[intonationPoints insertObject: tempPoint at:0];

	for j := 0; j < len(seq.IntonationPts) - 1; j++ {
		const IntonationPoint& point1 = seq.IntonationPts[j];
		const IntonationPoint& point2 = seq.IntonationPts[j + 1];

		double x1 = point1.absoluteTime() / 4.0;
		double y1 = point1.semitone() + 20.0;
		double m1 = point1.slope()

		double x2 = point2.absoluteTime() / 4.0;
		double y2 = point2.semitone() + 20.0;
		double m2 = point2.slope()

		double x12 = x1 * x1;
		double x13 = x12 * x1;

		double x22 = x2 * x2;
		double x23 = x22 * x2;

		double denominator = x2 - x1;
		denominator = denominator * denominator * denominator;

//		double d = ( -(y2 * x13) + 3 * y2 * x12 * x2 + m2 * x13 * x2 + m1 * x12 * x22 - m2 * x12 * x22 - 3 * x1 * y1 * x22 - m1 * x1 * x23 + y1 * x23 )
//			/ denominator;
		double c = ( -(m2 * x13) - 6 * y2 * x1 * x2 - 2 * m1 * x12 * x2 - m2 * x12 * x2 + 6 * x1 * y1 * x2 + m1 * x1 * x22 + 2 * m2 * x1 * x22 + m1 * x23 )
			/ denominator;
		double b = ( 3 * y2 * x1 + m1 * x12 + 2 * m2 * x12 - 3 * x1 * y1 + 3 * x2 * y2 + m1 * x1 * x2 - m2 * x1 * x2 - 3 * y1 * x2 - 2 * m1 * x22 - m2 * x22 )
			/ denominator;
		double a = ( -2 * y2 - m1 * x1 - m2 * x1 + 2 * y1 + m1 * x2 + m2 * x2) / denominator;

		insertEvent(32, point1.absoluteTime(), point1.semitone())
		//printf("Inserting Point %f\n", [point1 semitone])
		double yTemp = (3.0 * a * x12) + (2.0 * b * x1) + c;
		insertEvent(33, point1.absoluteTime(), yTemp)
		yTemp = (6.0 * a * x1) + (2.0 * b)
		insertEvent(34, point1.absoluteTime(), yTemp)
		yTemp = 6.0 * a;
		insertEvent(35, point1.absoluteTime(), yTemp)
	}
	//[intonationPoints removeObjectAt:0];

	//[self insertEvent:32 atTime: 0.0 withValue: -20.0]; /* A value of -20.0 in bin 32 should produce a
	//							    linear interp to -20.0 */
}

func (seq *Sequence) AddIntonationPoint(semiTone, offsetTime, slope float64, ruleIdx int {

	if ruleIdx > seq.CurRule {
		return;
	}

	IntonationPoint iPoint(this)
	iPoint.setRuleIndex(ruleIdx)
	iPoint.setOffsetTime(offsetTime)
	iPoint.setSemitone(semitone)
	iPoint.setSlope(slope)

	double time = iPoint.absoluteTime()
	for i := 0; i < len(seq.IntonationPts); i++ {
		if time < seq.IntonationPts[i].absoluteTime() {
			seq.IntonationPts.insert(seq.IntonationPts.begin() + i, iPoint)
			return;
		}
	}

	seq.IntonationPts.push_back(iPoint)
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
