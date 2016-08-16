package main

import (
	"math/rand"
	"testing"

	"github.com/gonum/plot"
	"github.com/gonum/plot/vg"
)

// TestBubbles tests the implementation of the bubbles
func TestBubbles(t *testing.T) {
	rand.Seed(int64(0))
	n := 10
	bubbleData := randomTriples(n)

	p, err := plot.New()
	if err != nil {
		panic(err)
	}
	p.Title.Text = "Bubbles"
	p.X.Label.Text = "X"
	p.Y.Label.Text = "Y"

	bs := NewBubbles(bubbleData, vg.Points(1), vg.Points(20))
	// bs.Color = color.RGBA{R: 196, B: 126, A: 255}
	p.Add(bs)

	p.X.Min = 0
	p.X.Max = 15
	p.Y.Min = 0
	p.Y.Max = 25

	if err := p.Save(4*vg.Inch, 4*vg.Inch, "../reports/figures/bubble.png"); err != nil {
		panic(err)
	}
}

// TestBlocks tests the implementation of the blocks
func TestBlocks(t *testing.T) {
	rand.Seed(int64(0))
	n := 16
	data := make(XYZs, n)
	for i := 0; i < n; i++ {
		data[i].X = float64(i / 4)
		data[i].Y = float64(i % 4)
		data[i].Z = rand.Float64()
	}
	blks := NewBlocks(data, vg.Points(1), vg.Points(20))
	p, err := plot.New()
	if err != nil {
		panic(err)
	}
	p.Title.Text = "Blocks"
	p.X.Label.Text = "X"
	p.Y.Label.Text = "Y"
	p.Add(blks)
	p.X.Min = 0
	p.X.Max = 4
	p.Y.Min = 0
	p.Y.Max = 4
	if err := p.Save(4*vg.Inch, 4*vg.Inch, "../reports/figures/blocks.png"); err != nil {
		panic(err)
	}
}

// TestBarChart tests the implementation of bar chart
func TestBarChart(t *testing.T) {
	rand.Seed(int64(0))
	n := 8
	vals := make([]float64, n)
	for i := 0; i < n; i++ {
		vals = append(vals, rand.Float64())
	}
	labels := make([]string, n)
	bars := NewBarChart(vals, labels)
	p, err := plot.New()
	if err != nil {
		panic(err)
	}
	p.Title.Text = "Bar Chart"
	p.X.Label.Text = "X"
	p.Y.Label.Text = "Y"
	p.Add(bars)
	p.X.Min = 0
	p.X.Max = float64(20)
	p.Y.Min = 0
	p.Y.Max = bars.MaxZ
	if err := p.Save(4*vg.Inch, 4*vg.Inch, "../reports/figures/bar_chart.png"); err != nil {
		panic(err)
	}
}

func randomTriples(n int) XYZs {
	data := make(XYZs, n)
	for i := range data {
		if i == 0 {
			data[i].X = rand.Float64()
		} else {
			data[i].X = data[i-1].X + 2*rand.Float64()
		}
		data[i].Y = data[i].X + 10*rand.Float64()
		data[i].Z = data[i].X
	}
	return data
}
