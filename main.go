package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"
)

// MBTA API response structures
type MBTAResponse struct {
	Data  []Prediction `json:"data"`
	Included []IncludedItem `json:"included"`
}

type Prediction struct {
	ID         string                 `json:"id"`
	Attributes PredictionAttributes   `json:"attributes"`
	Relationships PredictionRelationships `json:"relationships"`
}

type PredictionAttributes struct {
	ArrivalTime   *string `json:"arrival_time"`
	DepartureTime *string `json:"departure_time"`
}

type PredictionRelationships struct {
	Trip   *Relationship `json:"trip"`
	Route  *Relationship `json:"route"`
	Stop   *Relationship `json:"stop"`
}

type Relationship struct {
	Data *RelationshipData `json:"data"`
}

type RelationshipData struct {
	ID   string `json:"id"`
	Type string `json:"type"`
}

type IncludedItem struct {
	ID         string      `json:"id"`
	Type       string      `json:"type"`
	Attributes interface{} `json:"attributes"`
}

// Type-specific attribute structures
type TripAttributes struct {
	Headsign string `json:"headsign"`
}

type RouteAttributes struct {
	ShortName string `json:"short_name"`
	LongName  string `json:"long_name"`
}

type StopAttributes struct {
	Name string `json:"name"`
}

type TickerItem struct {
	Route     string
	Destination string
	TimeUntil string
}

// formatTime converts ISO time string to MM:SS format showing minutes and seconds until arrival
func formatTime(isoTime string) string {
	arrivalTime, err := time.Parse(time.RFC3339, isoTime)
	if err != nil {
		return "??:??"
	}

	duration := time.Until(arrivalTime)
	if duration < 0 {
		return "00:00"
	}

	minutes := int(duration.Minutes())
	seconds := int(duration.Seconds()) % 60
	return fmt.Sprintf("%d:%02d", minutes, seconds)
}

// findIncludedItem searches for an item in the included array by ID and type
func findIncludedItem(included []IncludedItem, itemType, itemID string) *IncludedItem {
	for _, item := range included {
		if item.Type == itemType && item.ID == itemID {
			return &item
		}
	}
	return nil
}

// getDestination extracts trip headsign from included trips
func getDestination(pred Prediction, included []IncludedItem) string {
	if pred.Relationships.Trip == nil || pred.Relationships.Trip.Data == nil {
		return "No Trip Data"
	}

	tripID := pred.Relationships.Trip.Data.ID
	trip := findIncludedItem(included, "trip", tripID)
	if trip == nil {
		return "No Matching Trip"
	}

	// Extract headsign from trip attributes
	if attrs, ok := trip.Attributes.(map[string]interface{}); ok {
		if headsign, exists := attrs["headsign"]; exists {
			if hs, ok := headsign.(string); ok {
				return hs
			}
		}
	}

	return "No Headsign"
}

// getRoute extracts route information from included routes
func getRoute(pred Prediction, included []IncludedItem) string {
	if pred.Relationships.Route == nil || pred.Relationships.Route.Data == nil {
		return "Unknown Route"
	}

	routeID := pred.Relationships.Route.Data.ID
	route := findIncludedItem(included, "route", routeID)
	if route == nil {
		return routeID
	}

	// Extract short_name from route attributes
	if attrs, ok := route.Attributes.(map[string]interface{}); ok {
		if shortName, exists := attrs["short_name"]; exists {
			if sn, ok := shortName.(string); ok && sn != "" {
				return sn
			}
		}
		// Fallback to long_name
		if longName, exists := attrs["long_name"]; exists {
			if ln, ok := longName.(string); ok && ln != "" {
				return ln
			}
		}
	}

	return routeID
}

// getStopName extracts stop name from included stops
func getStopName(included []IncludedItem, stopID string) string {
	stop := findIncludedItem(included, "stop", stopID)
	if stop == nil {
		return fmt.Sprintf("Stop %s", stopID)
	}

	// Extract name from stop attributes
	if attrs, ok := stop.Attributes.(map[string]interface{}); ok {
		if name, exists := attrs["name"]; exists {
			if n, ok := name.(string); ok && n != "" {
				return n
			}
		}
	}

	return fmt.Sprintf("Stop %s", stopID)
}

// formatTicker creates the ticker display from API response
func formatTicker(response *MBTAResponse, stopID string) string {
	stopName := getStopName(response.Included, stopID)

	var items []TickerItem

	// Process predictions (limit to 60 as in original)
	count := 0
	for _, pred := range response.Data {
		if count >= 60 {
			break
		}

		if pred.Attributes.ArrivalTime == nil {
			continue
		}

		route := getRoute(pred, response.Included)
		destination := getDestination(pred, response.Included)
		timeUntil := formatTime(*pred.Attributes.ArrivalTime)

		items = append(items, TickerItem{
			Route:       route,
			Destination: destination,
			TimeUntil:   timeUntil,
		})

		count++
	}

	if len(items) == 0 {
		return fmt.Sprintf("%s\nNo predictions available", stopName)
	}

	// Sort by arrival time (earliest first) - parse the time strings for proper sorting
	sort.Slice(items, func(i, j int) bool {
		// Parse MM:SS format
		var parseTime = func(timeStr string) int {
			parts := strings.Split(timeStr, ":")
			if len(parts) != 2 {
				return 9999 // Put invalid times at the end
			}
			minutes, _ := strconv.Atoi(parts[0])
			seconds, _ := strconv.Atoi(parts[1])
			return minutes*60 + seconds
		}
		return parseTime(items[i].TimeUntil) < parseTime(items[j].TimeUntil)
	})

	var lines []string
	lines = append(lines, stopName)

	for _, item := range items {
		line := fmt.Sprintf("  Route %s to %s arrives in %s",
			item.Route, item.Destination, item.TimeUntil)
		lines = append(lines, line)
	}

	return strings.Join(lines, "\n")
}

// fetchPredictions gets predictions from MBTA API
func fetchPredictions(stopID string) (*MBTAResponse, error) {
	// Try without route_type filter first to see if that helps
	url := fmt.Sprintf("https://api-v3.mbta.com/predictions?sort=arrival_time&filter[stop]=%s&include=trip,stop,route", stopID)

	resp, err := http.Get(url)
	if err != nil {
		return nil, fmt.Errorf("HTTP request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("API request failed with status: %d", resp.StatusCode)
	}

	var response MBTAResponse
	if err := json.NewDecoder(resp.Body).Decode(&response); err != nil {
		return nil, fmt.Errorf("JSON decode failed: %v", err)
	}

	return &response, nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: mbta-ticker <stop-id>")
		fmt.Println("Example: mbta-ticker place-pktrm")
		os.Exit(1)
	}

	stopID := os.Args[1]

	response, err := fetchPredictions(stopID)
	if err != nil {
		fmt.Printf("Error fetching predictions: %v\n", err)
		os.Exit(1)
	}

	ticker := formatTicker(response, stopID)
	fmt.Println(ticker)
}
