package main

import (
	"bufio"
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/jpeg"
	"math"
	"math/rand"
	"os"
	"time"

	"github.com/llgcode/draw2d"
	"github.com/llgcode/draw2d/draw2dimg"
	"github.com/llgcode/draw2d/draw2dkit"
	colorful "github.com/lucasb-eyer/go-colorful"
	"github.com/stretchr/stew/slice"

	// _ "golang.org/x/image/color"

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
	strokeColor    = color.RGBA{184, 150, 13, 255}
	fillColor      = color.RGBA{0x05, 0x05, 0x04, 0xff}
	lineWidthRatio = 0.15
	paddingRatio   = getPaddingRatio()
	discount       = paddingRatio + lineWidthRatio
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

func HSVtoRGBA(x, y, z float64) color.Color {
	c := colorful.Hsv(x, y, z)
	return color.RGBA{
		R: uint8(math.Floor(c.R * 255.0)),
		G: uint8(math.Floor(c.G * 255.0)),
		B: uint8(math.Floor(c.B * 255.0)),
		A: 0xff,
	}
}

func ToRGBA(c colorful.Color) color.Color {
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

func dot(a, b Vector2D) float64 {
	return a.x*b.x + a.y*b.y
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

func gridToImage(x, y float64) Vector2D {
	return Vector2D{float64(x * dotsPerGrid), float64(y * dotsPerGrid)}
}

type Curve struct {
	start, end string
	colors     map[int]color.Color
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

func shuffleVertices(rg []*Vertex) {
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

func getColor(i int) color.Color {
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
		colors := map[int]color.Color{}
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

func drawCurveOld(gc *draw2dimg.GraphicContext, dest *image.RGBA, cache map[string]*image.RGBA, gx, gy int, curve Curve) {
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
	var pos Vector2D
	if curve.start == "l" && curve.end == "r" {
		// Horizontal
		for i := 0; i <= lineCount; i += 1 {
			alpha := lerp(float64(i)/float64(lineCount), discount, 1.0-discount)
			pos = gridToImage(0.0, alpha)
			gc.BeginPath()
			gc.MoveTo(pos.x, pos.y)
			pos = gridToImage(1.0, alpha)
			gc.LineTo(pos.x, pos.y)

			gc.SetLineWidth(lineWidthRatio * dotsPerGrid)
			gc.SetStrokeColor(curve.colors[i])
			gc.Stroke()
		}
		return dest
	} else if curve.start == "b" && curve.end == "t" {
		// Vertical
		for i := 0; i <= lineCount; i += 1 {
			alpha := lerp(float64(i)/float64(lineCount), discount, 1.0-discount)
			pos = gridToImage(alpha, 0.0)
			gc.BeginPath()
			gc.MoveTo(pos.x, pos.y)
			pos = gridToImage(alpha, 1.0)
			gc.LineTo(pos.x, pos.y)

			gc.SetLineWidth(lineWidthRatio * dotsPerGrid)
			gc.SetStrokeColor(curve.colors[i])
			gc.Stroke()
		}
		return dest
	} else if curve.start == "b" && curve.end == "l" {
		// 6 to 9
		pos = gridToImage(0, 1)
	} else if curve.start == "b" && curve.end == "r" {
		// 6 to 3
		pos = gridToImage(1, 1)
	} else if curve.start == "l" && curve.end == "t" {
		// 9 to 12
		pos = gridToImage(0, 0)
	} else if curve.start == "r" && curve.end == "t" {
		// 3 to 12
		pos = gridToImage(1, 0)
	}
	if opaqueBackground {
		gc.SetFillColor(fillColor)
		draw2dkit.Circle(gc, pos.x, pos.y, dotsPerGrid*(1.0-discount))
		gc.Fill()
	}

	for i := 0; i <= lineCount; i += 1 {
		alpha := lerp(float64(i)/float64(lineCount), discount, 1.0-discount)

		if opaqueBackground {
			gc.SetLineWidth(dotsPerGrid * (lineWidthRatio + 2*paddingRatio))
			draw2dkit.Circle(gc, pos.x, pos.y, dotsPerGrid*alpha)

			gc.SetStrokeColor(fillColor)
			gc.Stroke()
		}

		draw2dkit.Circle(gc, pos.x, pos.y, dotsPerGrid*alpha)

		gc.SetLineWidth(lineWidthRatio * dotsPerGrid)
		if curve.colors[i] != nil {
			gc.SetStrokeColor(curve.colors[i])
		} else {
			gc.SetStrokeColor(strokeColor)
		}
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

type Vertex struct {
	name            string
	pos             Vector2D
	norm            Vector2D
	adjacentRegions []string
}

type Edge struct {
	regionName       string
	vertexA, vertexB *Vertex
	color            color.Color
}

func (edge *Edge) name() string {
	return edgeName(edge.vertexA.name, edge.vertexB.name)
}

func edgeName(vertexNameA, vertexNameB string) string {
	if vertexNameA < vertexNameB {
		return fmt.Sprintf("%s-%s", vertexNameA, vertexNameB)
	} else {
		return fmt.Sprintf("%s-%s", vertexNameB, vertexNameA)
	}
}

type Region struct {
	name         string
	pos          Vector2D
	vertices     []*Vertex
	vertexByName map[string]*Vertex
}

func nextName(name_index *int) string {
	*name_index += 1
	return fmt.Sprintf("%d", name_index)
}

func getRegionName(x, y int) string {
	return fmt.Sprintf("r(%d,%d)", x, y)
}

func lerp2D(alpha float64, p1, p2 Vector2D) Vector2D {
	return Vector2D{
		lerp(alpha, p1.x, p2.x),
		lerp(alpha, p1.y, p2.y),
	}
}

func getVertexName(pos Vector2D) string {
	return fmt.Sprintf("v(%d,%d)", int(pos.x), int(pos.y))
}

func (region *Region) appendVertices(verticesByName map[string]*Vertex, count int, posStart Vector2D, posEnd Vector2D, norm Vector2D, neighborRegionName string) {
	for i := 0; i <= lineCount; i++ {
		pos := lerp2D(float64(i)/float64(lineCount), posStart, posEnd)
		name := getVertexName(pos)
		vertex, ok := verticesByName[name]
		if !ok {
			vertex = &Vertex{
				name:            name,
				pos:             pos,
				norm:            norm,
				adjacentRegions: []string{region.name, neighborRegionName},
			}
		}
		// Add this vertex to the legal vertices for this region
		region.vertices = append(region.vertices, vertex)
		_, ok = region.vertexByName[getRegionVertexName(region, vertex)]
		if ok {
			panic(fmt.Sprintf("inserting previously existing vertex %s into region %s", vertex.name, region.name))
		}
		region.vertexByName[getRegionVertexName(region, vertex)] = vertex
	}
}

func pickRandomOtherVertex(vertices []*Vertex, vertex *Vertex) *Vertex {
	if len(vertices) < 2 {
		panic("Not enough vertices")
	}
	for {
		i := rand.Intn(len(vertices))
		if vertices[i] != vertex {
			return vertices[i]
		}
	}
}

func pickRandomUnusedVertex(edges map[string]*Edge, region *Region, exceptVertex *Vertex) *Vertex {
	// This could be made faster by maintaining a set
	i := 0
	for {
		vertex := pickRandomOtherVertex(region.vertices, exceptVertex)
		if vertex == nil {
			panic("asdf")
		}
		edge, ok := edges[getRegionVertexName(region, vertex)]
		if !ok {
			return vertex
		}
		fmt.Printf("vertex %v already has edge %s\n", *vertex, edge.name())
		i += 1
		if i > lineCount*4 {
			panic(fmt.Sprintf("This is taking too long (finding %v)\n", *vertex))
		}
	}
}

func getRegionVertexName(region *Region, vertex *Vertex) string {
	return fmt.Sprintf("%v/%v", region.name, vertex.name)
}

func fixNorms(posA, normA, posB, normB Vector2D) (Vector2D, Vector2D) {
	normalize(&normA)
	normalize(&normB)
	aToB := sub(posB, posA)
	normalize(&aToB)
	if dot(normA, aToB) < 0.0 {
		normA = scale(-1, normA)
	}
	if dot(normB, aToB) > 0.0 {
		normB = scale(-1, normB)
	}
	return normA, normB
}

func abs(f float64) float64 {
	if f < 0 {
		return -f
	} else {
		return f
	}
}
func drawCurve(gc *draw2dimg.GraphicContext, x1, y1, x2, y2, x3, y3, x4, y4, lineWidth float64, color color.Color) {
	gc.BeginPath()
	gc.MoveTo(x1, y1)
	gc.CubicCurveTo(x2, y2, x3, y3, x4, y4)
	// gc.LineTo(x2, y2)
	// gc.LineTo(x3, y3)
	// gc.LineTo(x4, y4)
	gc.SetLineWidth(lineWidth)
	gc.SetStrokeColor(fillColor)
	gc.Stroke()
}
func renderRegion(region *Region, edges map[string]*Edge) *image.RGBA {
	dest := image.NewRGBA(image.Rect(0, 0, dotsPerGrid, dotsPerGrid))
	gc := draw2dimg.NewGraphicContext(dest)
	gc.SetLineJoin(draw2d.RoundJoin)
	drawnEdges := map[string]bool{}
	gc.Translate(-region.pos.x, -region.pos.y)
	for _, vertex := range region.vertices {
		regionVertexName := getRegionVertexName(region, vertex)
		edge, ok := edges[regionVertexName]
		if !ok {
			panic(fmt.Sprintf("Could not find an edge for regionVertex %v vertex %v\n", regionVertexName, vertex))
		}
		if drawnEdges[edge.name()] {
			continue
		}
		drawnEdges[edge.name()] = true

		// Render edge

		normA, normB := fixNorms(edge.vertexA.pos, edge.vertexA.norm, edge.vertexB.pos, edge.vertexB.norm)
		scaleFactor := 0.15 * (abs(edge.vertexA.pos.x-edge.vertexB.pos.x) + abs(edge.vertexA.pos.y-edge.vertexB.pos.y))
		normA = scale(scaleFactor, normA)
		normB = scale(scaleFactor, normB)

		drawCurve(
			gc,
			edge.vertexA.pos.x, edge.vertexA.pos.y,
			edge.vertexA.pos.x+normA.x, edge.vertexA.pos.y+normA.y,
			edge.vertexB.pos.x+normB.x, edge.vertexB.pos.y+normB.y,
			edge.vertexB.pos.x, edge.vertexB.pos.y, 1.5*lineWidthRatio*dotsPerGrid, fillColor)

		drawCurve(gc,
			edge.vertexA.pos.x, edge.vertexA.pos.y,
			edge.vertexA.pos.x+normA.x, edge.vertexA.pos.y+normA.y,
			edge.vertexB.pos.x+normB.x, edge.vertexB.pos.y+normB.y,
			edge.vertexB.pos.x, edge.vertexB.pos.y, lineWidthRatio*dotsPerGrid, edge.color)

		gc.BeginPath()
		gc.MoveTo(edge.vertexA.pos.x, edge.vertexA.pos.y)
		gc.LineTo(edge.vertexA.pos.x+normA.x, edge.vertexA.pos.y+normA.y)
		gc.LineTo(edge.vertexB.pos.x+normB.x, edge.vertexB.pos.y+normB.y)
		gc.LineTo(edge.vertexB.pos.x, edge.vertexB.pos.y)
		gc.SetLineWidth(lineWidthRatio * dotsPerGrid)
		gc.SetStrokeColor(edge.color)
		gc.Stroke()

		// Draw norms
		// drawNorm(gc, edge.vertexA.pos, normA)
		// drawNorm(gc, edge.vertexB.pos, normB)

	}
	return dest
}

func drawNorm(gc *draw2dimg.GraphicContext, pos, norm Vector2D) {
	normalize(&norm)
	gc.BeginPath()
	gc.MoveTo(pos.x, pos.y)
	gc.LineTo(pos.x+(norm.x*dotsPerGrid)/5.0, pos.y+(norm.y*dotsPerGrid)/5.0)
	gc.SetLineWidth(0.3 * lineWidthRatio * dotsPerGrid)
	gc.SetStrokeColor(white)
	gc.Stroke()
}

// Fill in missing colors
func main() {
	alpha1 := lerp(0/float64(lineCount), discount, 1.0-discount)
	alpha2 := lerp(1/float64(lineCount), discount, 1.0-discount)
	if alpha2-alpha1 <= lineWidthRatio {
		panic(fmt.Sprintf("Not enough space between lines.\nSpace used by lines = %v", lineWidthRatio*lineCount))
	}
	size := gridToImage(gridWidth, gridHeight)

	// Initialize the graphic context on an RGBA image
	dest := image.NewRGBA(image.Rect(0, 0, int(size.x), int(size.y)))
	gc := draw2dimg.NewGraphicContext(dest)

	// Clear the background
	gc.SetFillColor(fillColor)
	gc.BeginPath()
	gc.MoveTo(0, 0)
	gc.LineTo(size.x, 0)
	gc.LineTo(size.x, size.y)
	gc.LineTo(0, size.y)
	gc.Close()
	gc.Fill()

	regions := []*Region{}
	regionByName := map[string]*Region{}
	verticesByName := map[string]*Vertex{}

	// Generate all of the regions.
	for y := 0; y < gridHeight; y++ {
		for x := 0; x < gridWidth; x++ {
			region := &Region{
				name:         getRegionName(x, y),
				pos:          gridToImage(float64(x), float64(y)),
				vertices:     []*Vertex{},
				vertexByName: map[string]*Vertex{},
			}
			regions = append(regions, region)
			regionByName[region.name] = region
		}
	}

	// For each region, generate its vertices.
	for y := 0; y < gridHeight; y++ {
		for x := 0; x < gridWidth; x++ {
			region, ok := regionByName[getRegionName(x, y)]
			if !ok {
				panic("Could not find region")
			}

			// NB: all coordinates are in dotsPerGrid units (not translated for the region, since that
			// translation happens later.
			// Top
			posStart := gridToImage(float64(x)+discount, float64(y+1))
			posEnd := gridToImage(float64(x+1)-discount, float64(y+1))
			norm := Vector2D{0.0, -1.0}
			region.appendVertices(verticesByName, lineCount, posStart, posEnd, norm, getRegionName(x, y+1))

			// Bottom
			posStart = gridToImage(float64(x)+discount, float64(y))
			posEnd = gridToImage(float64(x+1)-discount, float64(y))
			norm = Vector2D{0.0, 1.0}
			region.appendVertices(verticesByName, lineCount, posStart, posEnd, norm, getRegionName(x, y-1))

			// Left
			posStart = gridToImage(float64(x), float64(y)+discount)
			posEnd = gridToImage(float64(x), float64(y+1)-discount)
			norm = Vector2D{1.0, 0.0}
			region.appendVertices(verticesByName, lineCount, posStart, posEnd, norm, getRegionName(x-1, y))

			// Right
			posStart = gridToImage(float64(x+1), float64(y)+discount)
			posEnd = gridToImage(float64(x+1), float64(y+1)-discount)
			norm = Vector2D{-1.0, 0.0}
			region.appendVertices(verticesByName, lineCount, posStart, posEnd, norm, getRegionName(x+1, y))
		}
	}

	edges := map[string]*Edge{}

	for _, region := range regions {
		shuffleVertices(region.vertices)
		if len(region.vertices)%2 != 0 {
			panic("we need an even number of vertices")
		}
		for i := 0; i < len(region.vertices); i += 2 {
			vertex := region.vertices[i]
			otherVertex := region.vertices[i+1]
			fmt.Printf("trying to create an edge for %v within region %v\n", vertex.name, region.name)
			regionVertexName := getRegionVertexName(region, vertex)
			if _, ok := edges[regionVertexName]; ok {
				// This vertex already has an edge in this region.
				fmt.Printf("vertex %s already has edge %v with regionVertexName=%v\n", vertex.name, *edges[regionVertexName], regionVertexName)
				panic("this should not happen")
			}
			fmt.Printf("it looks like vertex %v does not have an edge\n", vertex.name)
			edge := &Edge{
				regionName: region.name,
				vertexA:    vertex,
				vertexB:    otherVertex,
				color:      strokeColor,
			}
			edges[regionVertexName] = edge
			fmt.Printf("edge(%s): %v %v\n", regionVertexName, edge.vertexA, edge.vertexB)
			edges[getRegionVertexName(region, otherVertex)] = edge
			fmt.Printf("edge(%s): %v %v\n", getRegionVertexName(region, otherVertex), edge.vertexA, edge.vertexB)
		}
	}

	/*
		edgeColors := map[string]bool{}
		for _, region := range regions {
			shuffleVertices(region.vertices)
			for i := 0; i < len(region.vertices); i += 2 {
			}
		}
	*/

	for _, region := range regions {
		regionImage := renderRegion(region, edges)
		r := image.Rectangle{
			Min: image.Point{int(region.pos.x), int(region.pos.y)},
			Max: image.Point{int(region.pos.x) + dotsPerGrid, int(region.pos.y) + dotsPerGrid},
		}
		draw.Draw(dest, r, regionImage, image.Point{0, 0}, draw.Over)
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
