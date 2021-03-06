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

package trm

import (
	"math"
)

// kaiser window params
const Beta = float64(5.658) // kaiser window parameters
const IZeroEpsilon = 1e-21

// Sample rate conversion constants
const ZeroCrossings = 13              // source cutoff frequency
const LpCutoff = float64(11.0 / 13.0) // 0.846 of nyquist
const FilterLength = ZeroCrossings * LRange

// const NBits = 16
const LBits = 8
const LRange = 256 // must be 2^L_BITS
const MBits = 8
const MRange = 256 // must be 2^M_BITS
const FractionBits = LBits + MBits
const FractionRange = 65536 // must be 2^FRACTION_BITS
const FilterLimit = FilterLength - 1

const NMask uint32 = 0xFFFF0000
const LMask uint32 = 0x0000FF00
const MMask uint32 = 0x000000FF
const FractionMask uint32 = 0x0000FFFF
const BufferSize = 1024  // ring buffer size
const OutputRate = 44100 // output sample rate (22.05, 44.1 KHz)

func nValue(x uint32) uint32 {
	return ((x) & NMask) >> FractionBits
}

func lValue(x uint32) uint32 {
	return ((x) & LMask) >> MBits
}

func mValue(x uint32) uint32 {
	return (x) & MMask
}

func fractionValue(x uint32) uint32 {
	return (x) & FractionMask
}

// RateConverter converts the sample rate
type RateConverter struct {
	SampleRateRatio  float64
	FillPtr          int32
	EmptyPtr         int32
	PadSize          int32
	FillSize         int32
	FillCounter      int32
	FilterIncrement  uint32
	PhaseIncrement   uint32
	TimeRegIncrement uint32
	TimeReg          uint32
	MaxSampleValue   float64
	NSamples         int64
	H                [FilterLength]float64
	DeltaH           [FilterLength]float64
	Buffer           [BufferSize]float64
	OutputData       *[]float64
}

// Init
func (src *RateConverter) Init(sampleRate int, outputRate int, outputData *[]float64) {
	//src.OutputData = append(src.OutputData, *outputData...)
	src.OutputData = outputData
	src.InitConversion(sampleRate, float64(outputRate))
}

// Reset resets some values of the converter
func (src *RateConverter) Reset() {
	src.EmptyPtr = 0
	src.TimeReg = 0
	src.FillCounter = 0
	src.MaxSampleValue = 0.0
	src.NSamples = 0
	src.InitBuffer()
}

// InitConversion initializes all the sample rate conversion functions
func (src *RateConverter) InitConversion(sampleRate int, outputRate float64) {
	src.InitFilter() // initialize filter impulse response

	src.SampleRateRatio = outputRate / float64(sampleRate)

	//  missing Round
	src.TimeRegIncrement = uint32(math.Round(math.Pow(2.0, float64(FractionBits)) / float64(src.SampleRateRatio)))

	roundedSampleRateRatio := math.Pow(2.0, FractionBits) / float64(src.TimeRegIncrement)

	if src.SampleRateRatio >= 1.0 {
		src.FilterIncrement = LRange
	} else {
		src.PhaseIncrement = uint32(math.Round(float64(src.SampleRateRatio) * FractionRange))
	}

	if src.SampleRateRatio >= 1.0 {
		src.PadSize = ZeroCrossings
	} else {
		src.PadSize = int32(float64(ZeroCrossings)/roundedSampleRateRatio) + 1
	}
	src.InitBuffer() // initialize the ring buffer
}

// IZero returns the value for the modified Bessel function of the first kind, order 0, as a float
func (src *RateConverter) IZero(x float64) float64 {
	var sum float64 = 1.0
	var u float64 = 1.0
	var halfx float64 = x / 2.0

	n := 1

	for {
		temp := halfx / float64(n)
		n += 1
		temp *= temp
		u *= temp
		sum += u
		if u < IZeroEpsilon*sum {
			break
		}
	}
	return sum
}

// InitBuffer initializes the ring buffer used for sample rate conversion
func (src *RateConverter) InitBuffer() {
	for i := 0; i < BufferSize; i++ {
		src.Buffer[i] = 0.0
	}
	src.FillPtr = src.PadSize
	src.FillSize = BufferSize - (2 * src.PadSize)
}

// InitFilter initializes filter impulse response and impulse delta values
func (src *RateConverter) InitFilter() {
	src.H[0] = LpCutoff
	x := math.Pi / float64(LRange)

	// initialize the filter impulse response
	for i := 1; i < FilterLength; i++ {
		y := float64(i) * x
		src.H[i] = math.Sin(float64(y)*float64(LpCutoff)) / y
	}

	// apply a kaiser window to the impulse response
	iBeta := 1.0 / src.IZero(Beta)
	for i := 0; i < FilterLength; i++ {
		temp := float64(i / FilterLength)
		src.H[i] *= src.IZero(Beta*math.Sqrt(float64(1.0)-(temp*temp))) * iBeta
	}

	for i := 0; i < FilterLimit; i++ {
		src.DeltaH[i] = src.H[i+1] - src.H[i]
	}
	src.DeltaH[FilterLimit] = 0.0 - src.H[FilterLimit]
}

// DataFill fills the ring buffer with a single sample, increments the counters and pointers,
// and empties the buffer when full
func (src *RateConverter) DataFill(data float64) {
	src.Buffer[src.FillPtr] = data
	SrIncrement(&src.FillPtr, BufferSize)
	src.FillCounter += 1
	if src.FillCounter >= src.FillSize {
		src.DataEmpty()
		src.FillCounter = 0
	}
}

// DataEmpty converts available portion of the input signal to the new sampling rate,
// and outputs the samples to the sound struct.
func (src *RateConverter) DataEmpty() {
	endPtr := src.FillPtr - src.PadSize

	if endPtr < 0 {
		endPtr += BufferSize
	}
	if endPtr < src.EmptyPtr {
		endPtr += BufferSize
	}
	// upsample loop (slightly more efficient than downsampling
	if src.SampleRateRatio >= 1.0 {
		for src.EmptyPtr < endPtr {
			output := float64(0.0)
			interpolation := float64(mValue(src.TimeReg)) / float64(MRange)

			// compute the left side of the filter convolution
			index := src.EmptyPtr
			for fidx := lValue(src.TimeReg); fidx < FilterLength; fidx += uint32(src.FilterIncrement) {
				SrDecrement(&index, BufferSize)
				output += (src.Buffer[index] * (src.H[fidx] + (src.DeltaH[fidx] * interpolation)))
			}

			// adjust values for right side calculation
			src.TimeReg = ^src.TimeReg // inverse of each bit
			interpolation = float64(mValue(src.TimeReg)) / float64(MRange)

			// compute the right side of the filter convolution
			index = src.EmptyPtr
			SrIncrement(&index, BufferSize)
			for fidx := lValue(src.TimeReg); fidx < FilterLength; fidx += uint32(src.FilterIncrement) {
				SrDecrement(&index, BufferSize)
				output += (src.Buffer[index] * (src.H[fidx] + (src.DeltaH[fidx] * interpolation)))
			}

			// record maximum sample value
			absoluteSampleValue := math.Abs(output)
			if absoluteSampleValue > src.MaxSampleValue {
				src.MaxSampleValue = absoluteSampleValue
			}

			src.NSamples += 1

			// save the sample
			*src.OutputData = append(*src.OutputData, output)

			// change time register back to original form
			src.TimeReg = ^src.TimeReg
			src.TimeReg += src.TimeRegIncrement

			// increment the empty pointer, adjusting it and end pointer
			src.EmptyPtr += int32(nValue(src.TimeReg))
			if src.EmptyPtr >= BufferSize {
				src.EmptyPtr -= BufferSize
				endPtr -= BufferSize
			}

			// clear n part of time register
			src.TimeReg &= ^NMask
		}
	} else {
		///*  DOWNSAMPLING CONVERSION LOOP  */
		//
		//while (emptyPtr_ < endPtr) {
		//
		//	/*  RESET ACCUMULATOR TO ZERO  */
		//	float output = 0.0;
		//
		//	/*  COMPUTE P PRIME  */
		//	unsigned int phaseIndex = (unsigned int) rint(
		//		((float) fractionValue(timeRegister_)) * sampleRateRatio_);
		//
		//	/*  COMPUTE THE LEFT SIDE OF THE FILTER CONVOLUTION  */
		//	int index = emptyPtr_;
		//	unsigned int impulseIndex;
		//	while ((impulseIndex = (phaseIndex >> M_BITS)) < FILTER_LENGTH) {
		//		float impulse = h_[impulseIndex] + (deltaH_[impulseIndex] *
		//			(((float) mValue(phaseIndex)) / (float) M_RANGE));
		//		output += (buffer_[index] * impulse);
		//		srDecrement(&index, BUFFER_SIZE);
		//		phaseIndex += phaseIncrement_;
		//	}
		//
		//	/*  COMPUTE P PRIME, ADJUSTED FOR RIGHT SIDE  */
		//	phaseIndex = (unsigned int) rint(
		//		((float) fractionValue(~timeRegister_)) * sampleRateRatio_);
		//
		//	/*  COMPUTE THE RIGHT SIDE OF THE FILTER CONVOLUTION  */
		//	index = emptyPtr_;
		//	srIncrement(&index, BUFFER_SIZE);
		//	while ((impulseIndex = (phaseIndex >> M_BITS)) < FILTER_LENGTH) {
		//		float impulse = h_[impulseIndex] + (deltaH_[impulseIndex] *
		//			(((float) mValue(phaseIndex)) / (float) M_RANGE));
		//		output += (buffer_[index] * impulse);
		//		srIncrement(&index, BUFFER_SIZE);
		//		phaseIndex += phaseIncrement_;
		//	}
		//
		//	/*  RECORD MAXIMUM SAMPLE VALUE  */
		//	float absoluteSampleValue = fabs(output);
		//	if (absoluteSampleValue > maximumSampleValue_) {
		//		maximumSampleValue_ = absoluteSampleValue;
		//	}
		//
		//	/*  INCREMENT SAMPLE NUMBER  */
		//	numberSamples_++;
		//
		//	/*  SAVE THE SAMPLE  */
		//	outputData_.push_back(static_cast<float>(output));
		//
		//	/*  INCREMENT THE TIME REGISTER  */
		//	timeRegister_ += timeRegisterIncrement_;
		//
		//	/*  INCREMENT THE EMPTY POINTER, ADJUSTING IT AND END POINTER  */
		//	emptyPtr_ += nValue(timeRegister_);
		//	if (emptyPtr_ >= BUFFER_SIZE) {
		//		emptyPtr_ -= BUFFER_SIZE;
		//		endPtr -= BUFFER_SIZE;
		//	}
		//
		//	/*  CLEAR N PART OF TIME REGISTER  */
		//	timeRegister_ &= (~N_MASK);
		//}
	}
}

// MaxSampleVal
func (src *RateConverter) MaxSampleVal() float64 {
	return src.MaxSampleValue
}

// SrIncrement increments the buffer position keeping it within the range 0 to (modulus - 1)
func SrIncrement(pos *int32, modulus int) {
	*pos += 1
	if *pos >= int32(modulus) {
		*pos -= int32(modulus)
	}
}

// SrDecrement decrements the buffer position keeping it within the range 0 to (modulus - 1)
func SrDecrement(pos *int32, modulus int) {
	*pos -= 1
	if *pos < 0 {
		*pos += int32(modulus)
	}
}

// FlushBuffer pads the buffer with zero samples, and flushes it by converting the remaining samples
func (src *RateConverter) FlushBuffer() {
	for i := 0; i < int(src.PadSize*2); i++ {
		src.DataFill(0.0)
	}
	src.DataEmpty()
}
