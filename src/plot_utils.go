package main

import (
	"errors"
	"fmt"
	"image/color"
	"math"

	"github.com/gonum/plot"
	"github.com/gonum/plot/vg"
	"github.com/gonum/plot/vg/draw"
)

//
// Adapted from
// github.com/gonum/plot/wiki/Creating-Custom-Plotters:-A-tutorial-on-creating-custom-Plotters
//

// XYZer wraps the Lne and XYZ methods
type XYZer interface {
	Len() int
	XYZ(int) (float64, float64, float64)
}

// XYZs implements the XYZer interface using a slice
type XYZs []struct{ X, Y, Z float64 }

// Len implements the Len method of the XYZer interface
func (xyz XYZs) Len() int {
	return len(xyz)
}

// XYZ implements the XYZ method of the XYZer interface
func (xyz XYZs) XYZ(i int) (float64, float64, float64) {
	return xyz[i].X, xyz[i].Y, xyz[i].Z
}

// CopyXYZs copies an XYZer
func CopyXYZs(data XYZer) XYZs {
	cpy := make(XYZs, data.Len())
	for i := range cpy {
		cpy[i].X, cpy[i].Y, cpy[i].Z = data.XYZ(i)
	}
	return cpy
}

// ColorScale represents a linear progression from one color to another
type ColorScale struct {
	min, max             float64
	StartColor, EndColor color.Color
}

// Color calculates
func (cs *ColorScale) Color(i float64) color.Color {
	if i <= cs.min {
		return cs.StartColor
	} else if i >= cs.max {
		return cs.EndColor
	} else {
		r1, g1, b1, a1 := cs.StartColor.RGBA()
		r2, g2, b2, a2 := cs.EndColor.RGBA()
		mr := float64(r2-r1) / (cs.max - cs.min)
		mg := float64(g2-g1) / (cs.max - cs.min)
		mb := float64(b2-b1) / (cs.max - cs.min)
		ma := float64(a2-a1) / (cs.max - cs.min)
		return color.RGBA{
			R: uint8((mr*(i-cs.min) + float64(r1)) / 257),
			G: uint8((mg*(i-cs.min) + float64(g1)) / 257),
			B: uint8((mb*(i-cs.min) + float64(b1)) / 257),
			A: uint8((ma*(i-cs.min) + float64(a1)) / 257),
		}
	}
}

// Bubbles implements the Plotter interface, drawing a bubble plot
// of x, y, z triples where the z value determines the radius of the
// bubble.
type Bubbles struct {
	XYZs
	Scale                ColorScale
	MinRadius, MaxRadius vg.Length
	MinZ, MaxZ           float64
}

// Blocks implements the Plotter interface for heatmap elements
type Blocks struct {
	XYZs
	Scale      ColorScale
	MinZ, MaxZ float64
}

// Bars implements the Plotter interface to create a bar chart
type Bars struct {
	Vals       []float64
	Color      color.Color
	MinZ, MaxZ float64
	Labels     []string
}

// NewBubbles creates as new bubble plot plotter for the given data,
// with a minimum and maximum bubble radius
func NewBubbles(xyz XYZer, min, max vg.Length) *Bubbles {
	cpy := CopyXYZs(xyz)
	minz := cpy[0].Z
	maxz := cpy[0].Z
	for _, d := range cpy {
		minz = math.Min(minz, d.Z)
		maxz = math.Max(maxz, d.Z)
	}
	return &Bubbles{
		XYZs: cpy,
		Scale: ColorScale{
			min:        minz,
			max:        maxz,
			StartColor: color.RGBA{R: 255, G: 1, B: 1, A: 255},
			EndColor:   color.RGBA{R: 255, G: 255, B: 1, A: 255},
		},
		MinRadius: min,
		MaxRadius: max,
		MinZ:      minz,
		MaxZ:      maxz,
	}
}

// NewBlocks creates a new block plot pltter for the given data
func NewBlocks(xyz XYZer, min, max vg.Length) *Blocks {
	cpy := CopyXYZs(xyz)
	minz := cpy[0].Z
	maxz := cpy[0].Z
	for _, d := range cpy {
		minz = math.Min(minz, d.Z)
		maxz = math.Max(maxz, d.Z)
	}
	return &Blocks{
		XYZs: cpy,
		MinZ: minz,
		MaxZ: maxz,
		Scale: ColorScale{
			min:        minz,
			max:        maxz,
			StartColor: color.RGBA{R: 255, G: 1, B: 1, A: 255},
			EndColor:   color.RGBA{R: 255, G: 255, B: 1, A: 255},
		},
	}
}

// NewHeatMap creates a new heatmap given the data
func NewHeatMap(vals []float64, x, y int) (*Blocks, error) {
	if len(vals) != x*y {
		return nil, errors.New("length of values does not equal x * y")
	}
	minz := vals[0]
	maxz := vals[0]
	xyz := make(XYZs, len(vals))
	for i, d := range vals {
		minz = math.Min(minz, d)
		maxz = math.Max(maxz, d)
		xyz[i].X = float64(i % x)
		xyz[i].Y = float64(i % y)
		xyz[i].Z = d
	}
	return &Blocks{
		XYZs: xyz,
		MinZ: minz,
		MaxZ: maxz,
		Scale: ColorScale{
			min:        minz,
			max:        maxz,
			StartColor: color.RGBA{255, 1, 1, 255},
			EndColor:   color.RGBA{255, 255, 1, 255},
		},
	}, nil
}

// NewBarChart creates a new bar chart
func NewBarChart(vals []float64, labels []string) *Bars {
	minz, maxz := getMinMax(vals)
	return &Bars{
		Vals:   vals,
		Color:  color.RGBA{R: 255, G: 0, B: 0, A: 255},
		MinZ:   minz,
		MaxZ:   maxz,
		Labels: labels,
	}
}

func getMinMax(vals []float64) (min, max float64) {
	min = vals[0]
	max = vals[0]
	for _, d := range vals {
		min = math.Min(min, d)
		max = math.Max(max, d)
	}
	return
}

// radius returns the radius of a bubble, in drawing units
// (vg.Lengths), by linear interpolation
func (bs *Bubbles) radius(z float64) vg.Length {
	if bs.MinZ == bs.MaxZ {
		return (bs.MaxRadius-bs.MinRadius)/2 + bs.MinRadius
	}

	// Convert MinZ and MaxZ to vg.Lengths. We just want them to compute
	// a slope so the units don't matter, and the conversion is okay
	minz := vg.Length(bs.MinZ)
	maxz := vg.Length(bs.MaxZ)
	slope := (bs.MaxRadius - bs.MinRadius) / (maxz - minz)
	intercept := bs.MaxRadius - maxz*slope
	return vg.Length(z)*slope + intercept
}

// Plot implements the Plot method of the plot.Plotter interface
func (bs *Bubbles) Plot(c draw.Canvas, plt *plot.Plot) {
	trX, trY := plt.Transforms(&c)

	for _, d := range bs.XYZs {
		// Transform the data x,y coordinate of this bubble to the
		// corresponding drawing coordinate
		x := trX(d.X)
		y := trY(d.Y)

		xx := color.RGBA{R: 255, G: 1, B: 1, A: 255}
		fmt.Println(xx.RGBA())

		fmt.Printf("%f: %s\n", d.Z, bs.Scale.Color(d.Z))
		c.SetColor(bs.Scale.Color(d.Z))

		// Get the radius of this bubble. The radius is specified in
		// drawing units so it does not need to be transformed
		rad := bs.radius(d.Z)

		// Fill a circle centered at x,y on the draw area
		var p vg.Path
		p.Move(vg.Point{X: x + rad, Y: y})
		p.Arc(vg.Point{X: x, Y: y}, rad, 0, 2*math.Pi)
		p.Close()
		c.Fill(p)
	}
}

// Plot implements the Plot method of the plot.Plotter interface for
// Blocks
func (blks *Blocks) Plot(c draw.Canvas, plt *plot.Plot) {
	trX, trY := plt.Transforms(&c)
	for _, d := range blks.XYZs {
		x := trX(d.X)
		y := trY(d.Y)
		c.SetColor(blks.Scale.Color(d.Z))
		var rect vg.Rectangle
		rect.Min = vg.Point{X: x, Y: y}
		rect.Max = vg.Point{X: x + trX(0.25), Y: y + trY(0.25)}
		c.Fill(rect.Path())
	}
}

// Plot implements the Plot method of the plot.Plotter interface for
// creating a bar chart
func (bars *Bars) Plot(c draw.Canvas, plt *plot.Plot) {
	trX, trY := plt.Transforms(&c)
	c.SetColor(bars.Color)
	for i, d := range bars.Vals {
		var rect vg.Rectangle
		fmt.Println(i)
		rect.Min = vg.Point{X: trX(float64(i)), Y: trY(0)}
		rect.Max = vg.Point{X: trX(float64(i) + 0.75), Y: trY(d)}
		c.Fill(rect.Path())
	}
}
