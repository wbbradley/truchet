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
	"github.com/llgcode/draw2d/draw2dkit"
	colorful "github.com/lucasb-eyer/go-colorful"
	"github.com/stretchr/stew/slice"
)

type Circle struct {
	center Vector2D
	radius float64
	color  color.RGBA
}

type CircleTree struct {
	papa   *Circle
	babies []*CircleTree
}

const (
	randomTheta     = true
	randomizeColors = false
	depthJump       = 1
	gridWidth       = 4
	gridHeight      = 4
	dotsPerGrid     = 300
	maxDepth        = 15
	thetaIncrement  = 0.0
	maxRadiusRatio  = 0.85
	minRadiusRatio  = 0.55
	increment       = 0.125 / 4.0
	fillCircles     = false
	jitter          = 0.0
)

var (
	black           = color.RGBA{0x0, 0x0, 0x0, 0xff}
	white           = color.RGBA{0xff, 0xff, 0xff, 0xff}
	genPalette      = genPalette2
	getCircleRadius = getCircleRadius1
	getStrokeWidth  = getStrokeWidth1
	palette         []colorful.Color
	treeNum         = 0
)

func init() {
	// seed := time.Now().UnixNano()
	rand.Seed(49)
	palette = genPalette(maxDepth) // colorful.WarmPalette(maxDepth)
}

func getStrokeWidth1(r float64) float64 {
	return 2.0
}

func getCircleRadius1(r float64) float64 {
	return r - 1.0
}

func getCircleRadius2(r float64) float64 {
	return r * 0.75
}

func getStrokeWidth2(r float64) float64 {
	return r * 0.3
}

func drawCircle(gc *draw2dimg.GraphicContext, c *Circle) {
	var center = c.center
	if jitter != 0.0 {
		r := rand.Float64() * jitter * c.radius
		angle := rand.Float64() * math.Pi * 2.0
		center.x += math.Cos(angle) * r
		center.y += math.Sin(angle) * r
	}
	gc.BeginPath()
	if fillCircles {
		gc.SetFillColor(c.color)
		draw2dkit.Circle(gc, center.x, center.y, c.radius)
		gc.Fill()
	} else {
		gc.SetStrokeColor(c.color)
		gc.SetLineWidth(getStrokeWidth(c.radius))
		draw2dkit.Circle(gc, center.x, center.y, getCircleRadius(c.radius))
		gc.Stroke()
	}
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

func dist(a, b *Circle) float64 {
	return pointDistance(a.center.x, a.center.y, b.center.x, b.center.y)
}

func calcMaxRadiusFrom(x, y float64, siblings []*CircleTree) float64 {
	radius := 10000000.0
	for _, sibling := range siblings {
		siblingCenterDist := pointDistance(x, y, sibling.papa.center.x, sibling.papa.center.y)
		if siblingCenterDist < sibling.papa.radius {
			return 0.0
		}
		radius = min(radius, siblingCenterDist-sibling.papa.radius)
	}
	return radius
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

func circlesIntersect(a, b *Circle) (Vector2D, Vector2D) {
	r1_squared := sqr(a.radius)
	r2_squared := sqr(b.radius)
	R := distance(a.center, b.center)
	R_squared := sqr(R)
	base := add(
		midpoint(a.center, b.center),
		scale(
			(r1_squared-r2_squared)/(2.0*R_squared),
			sub(b.center, a.center)))
	C := 0.5 * math.Sqrt(2.0*(r1_squared+r2_squared)/R_squared-sqr(r1_squared-r2_squared)/sqr(R_squared)-1.0)
	offset := scale(C, Vector2D{b.center.y - a.center.y, a.center.x - b.center.x})
	return add(base, offset), sub(base, offset)
}

func lerp(a, x0, x1 float64) float64 {
	return (x1-x0)*a + x0
}

func getRadius(r float64) float64 {
	return lerp(rand.Float64(), minRadiusRatio, maxRadiusRatio) * r
}

func getTheta(depth int) float64 {
	if randomTheta {
		return rand.Float64() * math.Pi * 2.0
	} else {
		return lerp(float64(depth)/float64(maxDepth), 0, math.Pi*2.0)
	}
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

func drawTree(gc *draw2dimg.GraphicContext, tree *CircleTree) {
	if tree == nil {
		return
	}
	drawCircle(gc, tree.papa)
	for _, baby := range tree.babies {
		drawTree(gc, baby)
	}
	// drawTree2(gc, tree)
}

func drawTree2(gc *draw2dimg.GraphicContext, tree *CircleTree) {
	if tree == nil {
		return
	}
	for _, baby := range tree.babies {
		gc.MoveTo(tree.papa.center.x, tree.papa.center.y)
		gc.LineTo(baby.papa.center.x, baby.papa.center.y)
		gc.SetStrokeColor(color.RGBA{0x22, 0x77, 0x24, 0xff})
		gc.SetLineWidth(2.0)
		gc.Stroke()
	}
}

func gridToImage(x, y float64) (float64, float64) {
	return float64(x * dotsPerGrid), float64(y * dotsPerGrid)
}

type Curve struct {
	start, end string
}

func shuffle(rg []string) {
	rand.Shuffle(len(rg), func(i, j int) {
		rg[i], rg[j] = rg[j], rg[i]
	})
}

func findOtherSide(side string) string {
	var choices []string
	if side == "t" {
		choices = []string{"b", "l", "r"}
	} else if side == "b" {
		choices = []string{"t", "l", "r"}
	} else if side == "r" {
		choices = []string{"t", "l", "b"}
	} else {
		choices = []string{"t", "r", "b"}
	}
	shuffle(choices)
	return choices[0]
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

func buildTileStack(uncovered []string) []Curve {
	stack := []Curve{}
	for {
		shuffle(uncovered)
		start := uncovered[0]
		uncovered = uncovered[1:]
		end := findOtherSide(start)
		uncovered = removeCovering(uncovered, covering(start, end))
		curve := Curve{start, end}
		if start > end {
			curve = Curve{end, start}
		}
		stack = append(stack, curve)
		if len(uncovered) == 0 {
			break
		}
	}
	return stack
}

func allSides() []string {
	return []string{"b", "t", "l", "r"}
}

func drawTile(gc *draw2dimg.GraphicContext, gx, gy int) {
	stack := buildTileStack(allSides())
	fmt.Printf("(%v, %v) %v\n", gx, gy, stack)
	for i := range stack {
		curve := stack[len(stack)-1-i]
		drawCurve(gc, gx, gy, curve)
	}
}

func drawCurve(gc *draw2dimg.GraphicContext, gx, gy int, curve Curve) {
	gc.SetStrokeColor(black)
	gc.SetLineWidth(5)
	x := 0.0
	y := 0.0
	angle := 0.0
	lineCount := 6
	if curve.start == "l" && curve.end == "r" {
		// Horizontal
		for i := 0.0; i <= float64(lineCount); i += 1.0 {
			x, y = gridToImage(float64(gx), float64(gy)+i/float64(lineCount))
			gc.BeginPath()
			gc.MoveTo(x, y)
			x, y = gridToImage(float64(gx+1), float64(gy)+i/float64(lineCount))
			gc.LineTo(x, y)
			gc.Stroke()
		}
		return
	} else if curve.start == "b" && curve.end == "t" {
		// Vertical
		for i := 0.0; i <= float64(lineCount); i += 1.0 {
			gc.SetFillColor(&color.RGBA{0xa0, 0xa0, 0xe0, 0xff})
			x, y = gridToImage(float64(gx)+i/float64(lineCount), float64(gy))
			gc.BeginPath()
			gc.MoveTo(x, y)
			x, y = gridToImage(float64(gx)+i/float64(lineCount), float64(gy+1))
			gc.LineTo(x, y)
			gc.Stroke()
		}
		return
	} else if curve.start == "b" && curve.end == "l" {
		// 6 to 9
		x, y = gridToImage(float64(gx), float64(gy+1))
		angle = math.Pi * 0.5
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
		angle = -math.Pi * 0.5
	}

	gc.SetFillColor(&color.RGBA{0xe0, 0xa0, 0xa0, 0xff})
	gc.BeginPath()
	gc.MoveTo(x, y)
	gc.ArcTo(x, y, dotsPerGrid*0.5, dotsPerGrid*0.5, angle, math.Pi*0.5)
	gc.Close()
	gc.Fill()
	gc.ArcTo(x, y, dotsPerGrid*0.5, dotsPerGrid*0.5, angle, math.Pi*0.5)
	gc.Stroke()
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

	for y := 0; y < gridHeight; y++ {
		for x := 0; x < gridWidth; x++ {
			drawTile(gc, x, y)
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
