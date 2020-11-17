package main

import (
	"bufio"
	"fmt"
	"image"
	"image/draw"
	"image/jpeg"
	"math"
	"math/rand"
	"os"
	"time"

	"github.com/llgcode/draw2d/draw2dimg"
	"github.com/llgcode/draw2d/draw2dkit"
	colorful "github.com/lucasb-eyer/go-colorful"
	"github.com/stretchr/stew/slice"

	// "golang.org/x/image/draw"
	"github.com/muesli/gamut"
)

const (
	randomTheta      = true
	randomizeColors  = false
	depthJump        = 1
	lineCount        = 3
	gridWidth        = 10
	gridHeight       = 10
	dotsPerGrid      = 180
	opaqueBackground = false
)

var (
	white          = color.RGBA{0xff, 0xff, 0xff, 0xff}
	strokeColor    = color.RGBA{84, 50, 3, 255}
	fillColor      = color.RGBA{0x05, 0x05, 0x04, 0xff}
	lineWidthRatio = 0.15
	paddingRatio   = getPaddingRatio()
	discount       = paddingRatio + 0.5*lineWidthRatio
	palette        = []color.Color{}
)

func getPaddingRatio() float64 {
	if opaqueBackground {
		return lineWidthRatio * 0.5
	} else {
		return 0.0
	}
}

func init() {
	seed := time.Now().UnixNano()
	rand.Seed(seed)
	var err error
	palette, err = gamut.Generate(lineCount+1, gamut.PastelGenerator{})
	if err != nil {
		panic(err)
	}
}

func MustParseHex(s string) colorful.Color {
	c, err := colorful.Hex(s)
	if err != nil {
		panic("MustParseHex: " + err.Error())
	}
	return c
}

func sqr(x float64) float64 {
	return x * x
}

func HSVtoRGBA(x, y, z float64) color.RGBA {
	c := colorful.Hsv(x, y, z)
	return color.RGBA{
		R: uint8(math.Floor(c.R * 255.0)),
		G: uint8(math.Floor(c.G * 255.0)),
		B: uint8(math.Floor(c.B * 255.0)),
		A: 0xff,
	}
}

func ToRGBA(c colorful.Color) color.RGBA {
	return color.RGBA{
		uint8(c.R * 255.0),
		uint8(c.G * 255.0),
		uint8(c.B * 255.0),
		0xff,
	}
}

func lerp(a, x0, x1 float64) float64 {
	return (x1-x0)*a + x0
}

func max(x, y float64) float64 {
	if x > y {
		return x
	} else {
		return y
	}
}
func min(x, y float64) float64 {
	if x < y {
		return x
	} else {
		return y
	}
}

type Vector2D struct {
	x, y float64
}

func sub(a, b Vector2D) Vector2D {
	return Vector2D{a.x - b.x, a.y - b.y}
}

func add(a, b Vector2D) Vector2D {
	return Vector2D{a.x + b.x, a.y + b.y}
}

func normalize(a *Vector2D) {
	dist := math.Sqrt(a.x*a.x + a.y*a.y)
	a.x /= dist
	a.y /= dist
}

func norm(a Vector2D) Vector2D {
	dist := math.Sqrt(a.x*a.x + a.y*a.y)
	return Vector2D{a.x / dist, a.y / dist}
}

func radiusForFiller(p, outerRadius float64) float64 {
	return math.Sin(p) * outerRadius
}

func invert(p Vector2D) Vector2D {
	return Vector2D{-p.x, -p.y}
}

func scale(s float64, p Vector2D) Vector2D {
	return Vector2D{s * p.x, s * p.y}
}

func midpoint(a, b Vector2D) Vector2D {
	return scale(0.5, add(a, b))
}

func gridToImage(x, y float64) (float64, float64) {
	return float64(x * dotsPerGrid), float64(y * dotsPerGrid)
}

type Curve struct {
	start, end string
	colors     map[int]color.RGBA
}

type Stack struct {
	gx, gy int
	curves []Curve
}

func shuffle(rg []string) {
	rand.Shuffle(len(rg), func(i, j int) {
		rg[i], rg[j] = rg[j], rg[i]
	})
}

func covering(start, end string) []string {
	covers := []string{}
	if start > end {
		// Sort to make the logic easier.
		start, end = end, start
	}
	if start == end {
		panic("Start and end are the same!")
	}

	covers = append(covers, start)
	covers = append(covers, end)
	if start == "l" && end == "r" {
		covers = append(covers, "b")
		covers = append(covers, "t")
	} else if start == "b" && end == "t" {
		covers = append(covers, "l")
		covers = append(covers, "r")
	}
	// fmt.Printf("covering(%v, %v) -> %v\n", start, end, covers)
	return covers
}

func removeCovering(uncovered []string, covered []string) []string {
	// Return a new set of uncovered sides
	newUncovered := []string{}
	for _, side := range uncovered {
		if !slice.ContainsString(covered, side) {
			newUncovered = append(newUncovered, side)
		}
	}
	// fmt.Printf("removeCovering(%v, %v) -> %v\n", uncovered, covered, newUncovered)
	return newUncovered
}

func color(i int) color.RGBA {
	r, g, b, a := palette[i].RGBA()

	return color.RGBA{uint8(r), uint8(g), uint8(b), uint8(a)}
}

func buildTileStack(gx, gy int) Stack {
	uncovered := allSides()
	stack := Stack{}
	stack.gx = gx
	stack.gy = gy
	stack.curves = []Curve{}
	shuffle(uncovered)
	for {
		start, end := uncovered[0], uncovered[1]
		uncovered = uncovered[2:]
		uncovered = removeCovering(uncovered, covering(start, end))
		colors := map[int]color.RGBA{}
		/*
			for i := 0; i <= lineCount; i++ {
				r, g, b, a := palette[i].RGBA()

				colors[i] = color.RGBA{uint8(r), uint8(g), uint8(b), uint8(a)}
			}
		*/
		curve := Curve{start, end, colors}
		if start > end {
			curve = Curve{end, start, colors}
		}
		stack.curves = append(stack.curves, curve)
		if len(uncovered) == 0 {
			break
		}
	}
	return stack
}

func allSides() []string {
	return []string{"b", "t", "l", "r"}
}

func drawCurve(gc *draw2dimg.GraphicContext, dest *image.RGBA, cache map[string]*image.RGBA, gx, gy int, curve Curve) {
	curve_key := fmt.Sprintf("%v", curve)
	img, ok := cache[curve_key]
	if !ok {
		img = renderCachedCurve(curve)
		fmt.Printf("Rendered cache curve: %v\n", curve)
		cache[curve_key] = img
	}
	r := image.Rectangle{
		Min: image.Point{gx * dotsPerGrid, gy * dotsPerGrid},
		Max: image.Point{(gx + 1) * dotsPerGrid, (gy + 1) * dotsPerGrid},
	}
	draw.Draw(dest, r, img, image.Point{0, 0}, draw.Over)
}

func renderCachedCurve(curve Curve) *image.RGBA {
	dest := image.NewRGBA(image.Rect(0, 0, dotsPerGrid, dotsPerGrid))
	gc := draw2dimg.NewGraphicContext(dest)
	x := 0.0
	y := 0.0
	if curve.start == "l" && curve.end == "r" {
		// Horizontal
		for i := 0; i <= lineCount; i += 1 {
			alpha := lerp(float64(i)/float64(lineCount), discount, 1.0-discount)
			x, y = gridToImage(0.0, alpha)
			gc.BeginPath()
			gc.MoveTo(x, y)
			x, y = gridToImage(1.0, alpha)
			gc.LineTo(x, y)

			gc.SetLineWidth(lineWidthRatio * dotsPerGrid)
			gc.SetStrokeColor(curve.colors[i])
			gc.Stroke()
		}
		return dest
	} else if curve.start == "b" && curve.end == "t" {
		// Vertical
		for i := 0; i <= lineCount; i += 1 {
			alpha := lerp(float64(i)/float64(lineCount), discount, 1.0-discount)
			x, y = gridToImage(alpha, 0.0)
			gc.BeginPath()
			gc.MoveTo(x, y)
			x, y = gridToImage(alpha, 1.0)
			gc.LineTo(x, y)

			gc.SetLineWidth(lineWidthRatio * dotsPerGrid)
			gc.SetStrokeColor(curve.colors[i])
			gc.Stroke()
		}
		return dest
	} else if curve.start == "b" && curve.end == "l" {
		// 6 to 9
		x, y = gridToImage(0, 1)
	} else if curve.start == "b" && curve.end == "r" {
		// 6 to 3
		x, y = gridToImage(1, 1)
	} else if curve.start == "l" && curve.end == "t" {
		// 9 to 12
		x, y = gridToImage(0, 0)
	} else if curve.start == "r" && curve.end == "t" {
		// 3 to 12
		x, y = gridToImage(1, 0)
	}
	if opaqueBackground {
		gc.SetFillColor(fillColor)
		draw2dkit.Circle(gc, x, y, dotsPerGrid*(1.0-discount))
		gc.Fill()
	}

	for i := 0; i <= lineCount; i += 1 {
		alpha := lerp(float64(i)/float64(lineCount), discount, 1.0-discount)

		if opaqueBackground {
			gc.SetLineWidth(dotsPerGrid * (lineWidthRatio + 2*paddingRatio))
			draw2dkit.Circle(gc, x, y, dotsPerGrid*alpha)

			gc.SetStrokeColor(fillColor)
			gc.Stroke()
		}

		draw2dkit.Circle(gc, x, y, dotsPerGrid*alpha)

		gc.SetLineWidth(lineWidthRatio * dotsPerGrid)
		gc.SetStrokeColor(curve.colors[i])
		gc.Stroke()
	}
	return dest
}

func stackKey(x, y int) string {
	return fmt.Sprintf("%v,%v", x, y)
}

type Position struct {
	x, y  int
	side  string
	from  string
	index int
}

func markStack(graph map[string]*Stack, position Position, color color.RGBA) {
	stack := graph[stackKey(x, y)]
	for _, curve := range stack.curves {
		if curve.
	}
}

// Fill in missing colors
func main() {
	alpha1 := lerp(0/float64(lineCount), discount, 1.0-discount)
	alpha2 := lerp(1/float64(lineCount), discount, 1.0-discount)
	if alpha2-alpha1 <= lineWidthRatio {
		panic(fmt.Sprintf("Not enough space between lines.\nSpace used by lines = %v", lineWidthRatio*lineCount))
	}
	width, height := gridToImage(gridWidth, gridHeight)

	// Initialize the graphic context on an RGBA image
	fmt.Printf("dest is %v, %v\n", width, height)
	dest := image.NewRGBA(image.Rect(0, 0, int(width), int(height)))
	gc := draw2dimg.NewGraphicContext(dest)

	// Clear the background
	gc.SetFillColor(fillColor)
	gc.BeginPath()
	gc.MoveTo(0, 0)
	gc.LineTo(width, 0)
	gc.LineTo(width, height)
	gc.LineTo(0, height)
	gc.Close()
	gc.Fill()

	stacks := []*Stack{}
	graph := map[string]*Stack{}
	for y := 0; y < gridHeight; y++ {
		for x := 0; x < gridWidth; x++ {
			stack := buildTileStack(x, y)
			stacks = append(stacks, &stack)
			graph[stackKey(x, y)] = &stack
		}
	}

	// Color a few curves
	x, y := rand.Intn(gridWidth), rand.Intn(gridHeight)
	markStack(graph, x, y, color(0))

	cache := map[string]*image.RGBA{}

	// for i := 1; i >= 0; i -= 1 {
	for i := 0; i < 2; i += 1 {
		for _, stack := range stacks {
			if len(stack.curves) > i {
				drawCurve(gc, dest, cache, stack.gx, stack.gy, stack.curves[i])
			}
		}
	}

	SaveToJpegFile(fmt.Sprintf("truchet-%v.jpg", time.Now().Unix()), dest)

}
func SaveToJpegFile(filePath string, m image.Image) error {
	// Create the file
	f, err := os.Create(filePath)
	if err != nil {
		return err
	}
	defer f.Close()
	// Create Writer from file
	b := bufio.NewWriter(f)
	// Write the image into the buffer
	err = jpeg.Encode(b, m, &jpeg.Options{
		Quality: 95,
	})
	if err != nil {
		return err
	}
	err = b.Flush()
	if err != nil {
		return err
	}
	return nil
}
