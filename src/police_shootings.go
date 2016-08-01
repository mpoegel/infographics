package main

import (
	"encoding/csv"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/gonum/plot"
	"github.com/gonum/plot/plotter"
	"github.com/gonum/plot/plotutil"
	"github.com/gonum/plot/vg"
	"github.com/mpoegel/gota/data-frame"
)

// plot the number of shootings per week
func plotShootingsPerWeek(d df.DataFrame) {
	// parse the date column into Time objects
	dateStrs := d.GetColumn("date").AsString()
	dates := make([]time.Time, len(dateStrs))
	for i := range dateStrs {
		bits := strings.Split(dateStrs[i], "-")
		year, _ := strconv.Atoi(bits[0])
		month, _ := strconv.Atoi(bits[1])
		day, _ := strconv.Atoi(bits[2])
		dates[i] = time.Date(year, time.Month(month), day, 0, 0, 0, 0, time.UTC)
	}
	// count the number of occurances per week
	span := dates[len(dates)-1].Sub(dates[0])
	hoursInWeek := 24 * 7.
	weeks := int(span.Hours() / hoursInWeek)
	bins := make(plotter.Values, weeks+1)
	pts := make(plotter.XYs, weeks+1)
	w := dates[0]
	b := 0
	for i := 0; i < len(dates); {
		if dates[i].Sub(w).Hours()/24 < 7 {
			bins[b]++
			i++
			pts[b].Y++
		} else {
			w = w.AddDate(0, 0, 7)
			b++
			pts[b].X = float64(b)
		}
	}
	p, err := plot.New()
	if err != nil {
		panic(err)
	}
	p.Title.Text = "Number of Shootings Per Week"
	// try plotting as a histogram
	h, err := plotter.NewHist(bins, len(bins))
	if err != nil {
		panic(err)
	}
	p.Add(h)
	if err = p.Save(4*vg.Inch, 4*vg.Inch, "reports/figures/shootings_per_week_hist.png"); err != nil {
		panic(err)
	}
	p, err = plot.New()
	p.Title.Text = "Number of Shootings Per Week"
	p.X.Label.Text = fmt.Sprintf("Weeks from %s", dates[0])
	p.Y.Label.Text = "Number of Shootings"
	// try plotting as a line graph
	err = plotutil.AddLinePoints(p, "Line", pts)
	if err != nil {
		panic(err)
	}
	if err = p.Save(4*vg.Inch, 4*vg.Inch, "reports/figures/shootings_per_week_line.png"); err != nil {
		panic(err)
	}
}

func printUniques(d df.DataFrame, colName string) {
	uniques := d.GetColumn(colName).Uniques()
	fmt.Printf("Unique values for %s\n", colName)
	for _, d := range uniques {
		fmt.Printf("'%s' ", d)
	}
	fmt.Printf("\n")
}

func main() {
	d := df.DataFrame{}

	// open the data file
	absPath, _ := filepath.Abs("data/wp-police-shootings.csv")
	csvfile, err := os.Open(absPath)
	if err != nil {
		panic(err)
	}
	r := csv.NewReader(csvfile)
	records, err := r.ReadAll()
	if err != nil {
		panic(err)
	}

	// load the data as columns
	types := df.T{
		"id":                      "int",
		"name":                    "string",
		"date":                    "string",
		"manner_of_death":         "string",
		"armed":                   "string",
		"age":                     "int",
		"gender":                  "string",
		"race":                    "string",
		"city":                    "string",
		"state":                   "string",
		"signs_of_mental_illness": "bool",
		"threat_level":            "string",
		"flee":                    "string",
		"body_camera":             "bool",
	}
	err = d.LoadAndParse(records, types)
	if err != nil {
		panic(err)
	}

	printUniques(d, "manner_of_death")
	printUniques(d, "threat_level")
	printUniques(d, "flee")
	printUniques(d, "armed")
	printUniques(d, "race")

	plotShootingsPerWeek(d)

}
