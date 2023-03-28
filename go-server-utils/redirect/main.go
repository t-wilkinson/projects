// TODO append footer with summary
// TODO sort results
// TODO don't print table; return results and export `PrintTable`
// TODO use rootdomains to calculate matches inside validateUrl func
package redirect

import (
	"fmt"
	"github.com/jedib0t/go-pretty/v6/table"
	"github.com/jedib0t/go-pretty/v6/text"
	"net/http"
	"net/url"
	"os"
	"regexp"
	"strings"
	"sync"
	"time"
)

const (
	reset   = "\u001b[0m"
	success = iota
	requestError
	urlMatchError
)

func red(text string) string   { return "\u001b[31m" + text + reset }
func green(text string) string { return "\u001b[32m" + text + reset }

type JsonData struct {
	Checks      map[string]map[string]Expect `json:"checks"`
	Rootdomains []string                     `json:"rootdomains"`
}

type Expect struct {
	Code int    `json:"code"`
	Url  string `json:"url"`
}

type validation struct {
	time     time.Duration
	status   int
	url      string
	expects  check
	received check
}

type check struct {
	url  *url.URL
	code int
}

func Run(data JsonData) {
	var wg sync.WaitGroup
	expectGroups := data.Checks
	rootdomains := regexp.MustCompile(fmt.Sprintf(`^(.*)(%s)$`, strings.Join(data.Rootdomains, "|")))
	results := make(map[string]chan validation)

	for group, urls := range expectGroups {
		wg.Add(1)
		rows := make(chan validation, len(urls))
		results[group] = rows
		go validateUrls(&wg, group, urls, rows)
	}

	wg.Wait()
	printTable(results, rootdomains)
}

func validateUrls(wgMain *sync.WaitGroup, group string, urls map[string]Expect, rows chan<- validation) {
	defer wgMain.Done()
	var wg sync.WaitGroup

	for url, expect := range urls {
		wg.Add(1)
		go validateUrl(&wg, url, expect, rows)
	}

	wg.Wait()
	close(rows)
}

func validateUrl(wg *sync.WaitGroup, startUrl string, expect Expect, rows chan<- validation) {
	defer wg.Done()

	expectURL, err := url.Parse(expect.Url)
	if err != nil {
		panic(err)
	}

	res, t := timeRequest(startUrl)
	expects := check{expectURL, expect.Code}
	received := check{res.Request.URL, res.StatusCode}
	switch {
	case expects.url.String() != received.url.String():
		rows <- validation{t, requestError, startUrl, expects, received}
	case expects.code != received.code:
		rows <- validation{t, urlMatchError, startUrl, expects, received}
	default:
		rows <- validation{t, success, startUrl, expects, received}
	}
}

func timeRequest(url string) (*http.Response, time.Duration) {
	t0 := time.Now()
	res, err := http.Get(url)
	t1 := time.Now()
	t := t1.Sub(t0)
	if err != nil {
		panic(err)
	}
	res.Body.Close()

	return res, t
}

func initTable() table.Writer {
	tab := table.NewWriter()

	tab.SetColumnConfigs([]table.ColumnConfig{
		{Number: 1, AlignHeader: text.AlignLeft, Align: text.AlignLeft},
		{Number: 2, AlignHeader: text.AlignCenter, Align: text.AlignLeft},
		{Number: 3, AlignHeader: text.AlignCenter, Align: text.AlignLeft},
		{Number: 4, AlignHeader: text.AlignCenter, Align: text.AlignRight},
		{Number: 5, AlignHeader: text.AlignCenter, Align: text.AlignCenter},
		{Number: 6, AlignHeader: text.AlignCenter, Align: text.AlignRight},
		{Number: 7, AlignHeader: text.AlignCenter, Align: text.AlignCenter},
		{Number: 8, AlignHeader: text.AlignCenter, Align: text.AlignLeft},
	})

	tab.SetOutputMirror(os.Stdout)
	// tab.SortBy([]table.SortBy{
	// 	{Number: 2, Mode: table.Asc},
	// })
	tab.SetStyle(table.StyleLight)
	tab.Style().Options = table.Options{
		DrawBorder:      true,
		SeparateColumns: false,
		SeparateHeader:  true,
	}

	return tab
}

func printTable(results map[string]chan validation, rootdomains *regexp.Regexp) {
	tab := initTable()
	tab.AppendHeader([]interface{}{"Status", "MS", "URL", "Code", "Scheme", "Sub", "Host", "Path"})

	for group, rows := range results {
		tab.AppendSeparator()
		tab.AppendRow([]interface{}{group}, table.RowConfig{AutoMerge: true})
		appendRows(tab, rows, rootdomains)
	}

	tab.Render()
}

func appendRows(tab table.Writer, rows <-chan validation, rootdomains *regexp.Regexp) {
	append := func(status string, row validation, c check) {
		matches := rootdomains.FindStringSubmatch(c.url.Host)
		if len(matches) == 3 {
			tab.AppendRow([]interface{}{status, fmt.Sprintf("%dms", row.time.Milliseconds()), row.url, c.code, c.url.Scheme, matches[1], matches[2], c.url.Path})
		} else {
			tab.AppendRow([]interface{}{status, fmt.Sprintf("%dms", row.time.Milliseconds()), row.url, c.code, c.url.Scheme, "", c.url.Host, c.url.Path})
		}
	}

	for row := range rows {
		switch row.status {
		case success:
			append(green("success"), row, row.received)
		case requestError, urlMatchError:
			append(red("expects"), row, row.expects)
			append(red("recieved"), row, row.received)
		}
	}
}
