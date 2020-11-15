package main

import (
	"bufio"
	"fmt"
	"image"
	"image/color"
	"image/jpeg"
	"math"
	"math/rand"
	"os"
	"time"

	"github.com/llgcode/draw2d/draw2dimg"
	colorful "github.com/lucasb-eyer/go-colorful"
	"github.com/stretchr/stew/slice"
)

const (
	randomTheta     = true
	randomizeColors = false
	depthJump       = 1
	lineWidth       = 0.05 * dotsPerGrid
	lineCount       = 8
	gridWidth       = 10
	gridHeight      = 10
	dotsPerGrid     = 300
	maxDepth        = 15
	thetaIncrement  = 0.0
	increment       = 0.125 / 4.0
	fillCircles     = false
	jitter          = 0.0
)

var (
	black = color.RGBA{0x45, 0x15, 0x34, 0xff}
	white = ToRGBA(MustParseHex("#9e0142"))
	//.color.RGBA{0xa8, 0x25, 0xd5, 0xff}
	genPalette = genPalette2
	palette    []colorful.Color
	treeNum    = 0
)

func init() {
	seed := time.Now().UnixNano()
	rand.Seed(seed)
	// rand.Seed(49)
	palette = genPalette(maxDepth) // colorful.WarmPalette(maxDepth)
}

type GradientTable []struct {
	Col colorful.Color
	Pos float64
}

func genPalette1(d int) []colorful.Color {
	hue := rand.Float64() * 360.0
	return []colorful.Color{
		colorful.Color{
			R: 1.0,
			G: 1.0,
			B: 1.0,
		},
		colorful.Hsv(hue, 0.3, 0.9),
	}
}
func genPalette5(d int) []colorful.Color {
	hue := rand.Float64() * 360.0
	return []colorful.Color{
		colorful.Hsv(hue, 0.7, 0.95),
	}
}
func genPalette2(d int) []colorful.Color {
	hue := rand.Float64() * 360.0
	keypoints := GradientTable{
		{colorful.Hsv(hue, 0.3, 0.9), 0.0},
		{colorful.Hsv(hue, 0.3, 0.4), 0.25},
		{colorful.Hsv(hue, 0.05, 1.0), 0.5},
		{colorful.Hsv(hue, 0.3, 0.4), 0.75},
		{colorful.Hsv(hue, 0.2, 0.3), 1.0},
	}
	p := make([]colorful.Color, 0, d)
	for i := 0; i < d; i++ {
		p = append(p, keypoints.GetInterpolatedColorFor(float64(i)/float64(d)))
	}

	return p
}

func genPalette3(d int) []colorful.Color {
	keypoints := GradientTable{
		{MustParseHex("#9e0142"), 0.0},
		{MustParseHex("#d53e4f"), 0.1},
		{MustParseHex("#f46d43"), 0.2},
		{MustParseHex("#fdae61"), 0.3},
		{MustParseHex("#fee090"), 0.4},
		{MustParseHex("#ffffbf"), 0.5},
		{MustParseHex("#e6f598"), 0.6},
		{MustParseHex("#abdda4"), 0.7},
		{MustParseHex("#66c2a5"), 0.8},
		{MustParseHex("#3288bd"), 0.9},
		{MustParseHex("#5e4fa2"), 1.0},
	}
	p := make([]colorful.Color, 0, d)
	for i := 0; i < d; i++ {
		p = append(p, keypoints.GetInterpolatedColorFor(float64(i)/float64(d)))
	}

	return p
}

func genPalette4(d int) []colorful.Color {
	keypoints := GradientTable{
		{MustParseHex("#fe8282"), 0.0},
		{MustParseHex("#fe6262"), 0.3},
		{MustParseHex("#eeebee"), 0.5},
		{MustParseHex("#fe6262"), 0.8},
		{MustParseHex("#eeebee"), 1.0},
	}
	p := make([]colorful.Color, 0, d)
	for i := 0; i < d; i++ {
		p = append(p, keypoints.GetInterpolatedColorFor(float64(i)/float64(d)))
	}

	return p
}

func MustParseHex(s string) colorful.Color {
	c, err := colorful.Hex(s)
	if err != nil {
		panic("MustParseHex: " + err.Error())
	}
	return c
}

func (self GradientTable) GetInterpolatedColorFor(t float64) colorful.Color {
	for i := 0; i < len(self)-1; i++ {
		c1 := self[i]
		c2 := self[i+1]
		if c1.Pos <= t && t <= c2.Pos {
			// We are in between c1 and c2. Go blend them!
			t := (t - c1.Pos) / (c2.Pos - c1.Pos)
			return c1.Col.BlendHcl(c2.Col, t).Clamped()
		}
	}

	// Nothing found? Means we're at (or past) the last gradient keypoint.
	return self[len(self)-1].Col
}

func pointDistance(x1, y1, x2, y2 float64) float64 {
	return math.Sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))
}

func distance(a, b Vector2D) float64 {
	return pointDistance(a.x, a.y, b.x, b.y)
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

func randColor(depth int) color.RGBA {
	if randomizeColors {
		return HSVtoRGBA(rand.Float64()*360.0, 0.5, 0.9)
	} else {
		return ToRGBA(palette[depth%len(palette)])
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
		curve := Curve{start, end}
		if start > end {
			curve = Curve{end, start}
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

func drawCurve(gc *draw2dimg.GraphicContext, gx, gy int, curve Curve) {
	gc.SetStrokeColor(black)
	gc.SetLineWidth(lineWidth)
	x := 0.0
	y := 0.0
	angle := 0.0
	discount := 0.075
	padding := discount * dotsPerGrid
	if curve.start == "l" && curve.end == "r" {
		// Horizontal
		for i := 0.0; i <= float64(lineCount); i += 1.0 {
			alpha := lerp(i/float64(lineCount), discount, 1.0-discount)
			x, y = gridToImage(float64(gx), float64(gy)+alpha)
			gc.BeginPath()
			gc.MoveTo(x, y)
			x, y = gridToImage(float64(gx+1), float64(gy)+alpha)
			gc.LineTo(x, y)
			gc.Stroke()
		}
		return
	} else if curve.start == "b" && curve.end == "t" {
		// Vertical
		for i := 0.0; i <= float64(lineCount); i += 1.0 {
			alpha := lerp(i/float64(lineCount), discount, 1.0-discount)
			x, y = gridToImage(float64(gx)+alpha, float64(gy))
			gc.BeginPath()
			gc.MoveTo(x, y)
			x, y = gridToImage(float64(gx)+alpha, float64(gy+1))
			gc.LineTo(x, y)
			gc.Stroke()
		}
		return
	} else if curve.start == "b" && curve.end == "l" {
		// 6 to 9
		x, y = gridToImage(float64(gx), float64(gy+1))
		angle = -math.Pi * 0.5
	} else if curve.start == "b" && curve.end == "r" {
		// 6 to 3
		x, y = gridToImage(float64(gx+1), float64(gy+1))
		angle = -math.Pi
	} else if curve.start == "l" && curve.end == "t" {
		// 9 to 12
		x, y = gridToImage(float64(gx), float64(gy))
		angle = 0.0
	} else if curve.start == "r" && curve.end == "t" {
		// 3 to 12
		x, y = gridToImage(float64(gx+1), float64(gy))
		angle = math.Pi * 0.5
	}
	// gc.SetFillColor(&color.RGBA{0xe0, 0xa0, 0xa0, 0xff})
	for i := float64(lineCount); i >= 0.0; i -= 1.0 {
		alpha := lerp(i/float64(lineCount), discount, 1.0-discount)
		gc.BeginPath()
		gc.MoveTo(x, y)
		gc.ArcTo(x, y, dotsPerGrid*alpha, dotsPerGrid*alpha, angle, math.Pi*0.5)
		gc.Close()
		gc.Fill()

		gc.SetLineWidth(padding * 2)
		gc.SetStrokeColor(white)
		gc.ArcTo(x, y, dotsPerGrid*alpha, dotsPerGrid*alpha, angle, math.Pi*0.5)
		gc.Stroke()
		gc.SetLineWidth(lineWidth)
		gc.SetStrokeColor(black)
		extra := 0.02
		gc.ArcTo(x, y, dotsPerGrid*alpha, dotsPerGrid*alpha, angle-extra, math.Pi*0.5+extra*2.0)
		gc.Stroke()
	}
}

func main() {
	width, height := gridToImage(gridWidth, gridHeight)

	// Initialize the graphic context on an RGBA image
	dest := image.NewRGBA(image.Rect(0, 0, int(width), int(height)))
	gc := draw2dimg.NewGraphicContext(dest)

	// Clear the background
	gc.SetFillColor(white)
	gc.BeginPath()
	gc.MoveTo(0, 0)
	gc.LineTo(width, 0)
	gc.LineTo(width, height)
	gc.LineTo(0, height)
	gc.Close()
	gc.Fill()

	stacks := []Stack{}

	for y := 0; y < gridHeight; y++ {
		for x := 0; x < gridWidth; x++ {
			stacks = append(stacks, buildTileStack(x, y))
		}
	}

	// for i := 1; i >= 0; i -= 1 {
	for i := 0; i < 2; i += 1 {
		for _, stack := range stacks {
			if len(stack.curves) > i {
				drawCurve(gc, stack.gx, stack.gy, stack.curves[i])
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
