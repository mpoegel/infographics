package main

import (
	"bufio"
	"io"
	"log"
	"net/http"
	"os"
)

const wpPoliceShootings = "https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv"
const pgPoliceShootings2015 = "https://s3.amazonaws.com/postgraphics/policeshootings/policeshootings2015.json"
const pgPoliceShootings2016 = "https://s3.amazonaws.com/postgraphics/policeshootings/policeshootings2016.json"

// Pull a data source down from a uri and save it in ./data
func fetchData(uri string, outName string) {
	response, err := http.Get(uri)
	if err != nil {
		log.Fatal(err)
	} else {
		defer response.Body.Close()
		// create the data directory if it does not already exist
		if err := os.Mkdir("data", os.ModeDir); err != nil {
			log.Printf("data directory already exists")
		}
		// create the output file
		fo, err := os.Create("data/" + outName)
		if err != nil {
			panic(err)
		}
		defer func() {
			if err := fo.Close(); err != nil {
				panic(err)
			}
		}()
		// make a write buffer
		w := bufio.NewWriter(fo)
		// make a read buffer
		buf := make([]byte, 1024)
		for {
			n, err := response.Body.Read(buf)
			if err != nil && err != io.EOF {
				panic(err)
			}
			if n == 0 {
				break
			}
			// write to the out file buffer
			if _, err := w.Write(buf[:n]); err != nil {
				panic(err)
			}
		}
		if err := w.Flush(); err != nil {
			panic(err)
		}
	}
}

func main() {
	fetchData(wpPoliceShootings, "wp-police-shootings.csv")
	fetchData(pgPoliceShootings2015, "pg-police-shootings-2015.json")
	fetchData(pgPoliceShootings2016, "pg-police-shootings-2016.json")
}
