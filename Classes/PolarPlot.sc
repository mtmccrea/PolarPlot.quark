/*
TODO
- \circle data points, for the option of re-creating rE-style directivity displays
- invert values so that minimum values are at the radius, max values are at the center
- optionally resample the data when plotting for radially uniform sample points, instead of
-      one point per data point
- calculate plot/view dimensions based on actual plot outline (if it's not a circle)
-      so plot is maximized no matter the shape, e.g. a semi-circular or wedge plot
- optionally show lattitude scale in a separate, adjacent view that aligns with plot
- toggle data channels on and off
*/

PolarView : ValuesView {
	// set in drawFunc, for access by drawing layers
	var <bnds, <cen, <minDim;
	// layers
	var <plots, <grid, <legend, <title;
	var <thetas, data; // data points and their corresponding theta positions

	var <thetaDirection, <dirFlag;  // dirFlag: cw=1, ccw=-1
	var thetaMin, thetaMax, thetaRange; // min, max, range of the theta axis of the plot
	var <rhoMin, <rhoMax;    // min and max rho of the plot, can be nil (auto), if not nil, it will clip data. Note: different than plotMin/Max
	var thetaZeroPosition;
	var <prZeroPos;        // position of zero used internally, provides the point of reference for prStartAngle
	var <prStartAngle;     // start angle used internally, reference 0 to the RIGHT, as used in addAnnularWedge
	var <prThetaRange;     // sweep length used internally, = thetaRange * dirFlag
	var <plotRadius;       // normalized, 1 = outer edge of the view
	var <plotUnits;        // units in which the data is plotted
	var <plotData;         // data to plot, stored as scalar values, normalized to rhoMin/Max (unmapped by spec)
	var <rhoSpec;          // ControlSpec that maps data to plotting range
	var <scalarData;       // input data, stored as scalar values
	var <dataScalarMin, <dataScalarMax;  // min/max of input data, in scalar values
	var <rhoGridLines;     // values at which level grid lines are drawn, in plotUnits
	var <rhoGridLinesNorm; // rhoGridLines, normalized to rhoMin/Max
	var <thetaGridLines;   // theta positions of longitude lines, relative to 'thetaZeroPosition' and 'thetaDirection';
	var <prThetaGridLines; // theta positions of longitude lines, in drawing coordinates
	var <prPlotCen;        // center of the plot, offset by the title
	var <prPlotRad;        // radius of the plot, scaled by plotRadius arg

	var <>clipDbLow = -90; // min dB level when setting plot minimum (0.ampdb = -inf, which breaks ControlSpec);
	var <bipolar = false;  // display the data as the absolute values (of the scalar values), with negative values a different color
	var thetaGridLineSpacing;

	*new {
		|parent, bounds, data = ([0]), thetaArray = ([0, 2pi]), thetaBounds = ([0, 2pi]), rhoBounds, thetaDirection = \cw, thetaZeroPosition = \top, plotRadius = 0.9, dataUnits = \scalar, plotUnits, bipolar = false|
		^super.new(parent, bounds, [thetaBounds.asSpec], data).init(
			thetaArray, thetaBounds, rhoBounds, thetaDirection, thetaZeroPosition, plotRadius, dataUnits, plotUnits, bipolar
		);
	}


	init { |argThetaArray, argThetaBounds, argRhoBounds, argThetaDirection, argThetaZeroPosition, argPlotRadius, argDataUnits, argPlotUnits, argBipolar|

		// REQUIRED: in subclass init, initialize drawing layers
		layers = [
			PolarPlotLayer, PolarGridLayer,
			PolarLegendLayer, PolarTitleLayer
		].collect({ |class|
			class.new(this, class.properties)
		});

		// unpack the layers list into individual variables
		#plots, grid, legend, title = layers;

		plotRadius = argPlotRadius;
		plotUnits = argPlotUnits ?? { argDataUnits };
		thetaDirection = argThetaDirection;
		dirFlag = switch (thetaDirection, \cw, {1}, \ccw, {-1});
		thetaZeroPosition = if (argThetaZeroPosition.isKindOf(Number)) {argThetaZeroPosition} {
			switch(argThetaZeroPosition,
				\top,    { 0 },
				\bottom, { pi },
				\left,   { -0.5pi * dirFlag },
				\right,  { 0.5pi * dirFlag }
			)
		};

		thetaMin = argThetaBounds.minItem;
		thetaMax = argThetaBounds.maxItem;
		thetaRange = thetaMax - thetaMin;

		argRhoBounds !? {
			if (argRhoBounds.includes(nil)) {
				#rhoMin, rhoMax = argRhoBounds;
			} {
				rhoMin = argRhoBounds.minItem;
				rhoMax = argRhoBounds.maxItem;
			};
		};
		if (plotUnits == \db and: { rhoMin.notNil }) {
			rhoMin = max(rhoMin, clipDbLow)
		};


		bipolar = argBipolar;
		// this.prCalcDataBounds;
		// this.rhoMinMax_(rhoMin, rhoMax, refresh: false); // sets rhoSpec, nil for min or max will detect from data
		//
		// rhoSpec = switch (plotUnits,
		// 	\db,     { [-70, 0, \lin] },
		// 	\scalar, { [0,1] }
		// ).asSpec;


		// TODO: should this be actual data min / max nor the spec?
		// dataScalarMin = rhoSpec.minval;
		// dataScalarMax = rhoSpec.maxval;


		this.data_(values, argThetaArray, argDataUnits, bipolar, refresh: false);

		// rhoGridLines = (rhoSpec.minval, rhoSpec.minval + ((rhoSpec.maxval-rhoSpec.minval)/5) .. rhoSpec.maxval);
		// rhoGridLinesNorm = rhoGridLines.collect(rhoSpec.unmap(_));

		// TODO:
		// // ensure data encoded with correct units
		// rhoSpec = specs;
		// this.data_(values,
		// 	[].collect({|spc| [spc.minval, spc.maxval]}),
		// argDataUnits, argBipolar, refresh: false);

		thetaGridLineSpacing = pi/6;
		this.thetaDirection_(thetaDirection); // sets thetaZeroPosition, thetaMin, thetaRange
		this.thetaGridLines_(thetaGridLineSpacing, false);
		this.defineMouseActions;
	}


	drawFunc {
		^{  |v|
			var vw, vh, pw, ph; // view width, height, plot width, height
			var titleOffset;
			var plotBnds, plotOrigin, originOffset;
			var viewHRatio, plotRatio;

			// "global" instance vars, accessed by ValueViewLayers
			bnds = v.bounds;
			cen = bnds.center; // center of the view, NOT offset by the space used by the title
			if (title.p.show) {
				title.calcBounds;
				titleOffset = 2 * title.p.inset + title.bgRect.height;
				vh = bnds.height - titleOffset;
			} {
				titleOffset = 0;
				vh = bnds.height
			};
			vw = bnds.width;

			// TODO: the following calculation of the center of the plot is close
			// but not quite right for plots with sweep length of less than 2 pi
			// only noticeable in certain view size ratios.

			// get bounding rect of plot (normalized -1 > 1) for max width or height of 2
			// origin of this rect is the plot origin offset.
			plotBnds = this.prPlotBounds;
			plotOrigin = plotBnds.origin;
			pw = plotBnds.width;
			ph = plotBnds.height;
			viewHRatio = vh/vw;
			plotRatio = ph/pw;
			minDim = min(vw, vh);
			prPlotRad = if (plotRatio > viewHRatio) {
				vh / ph * plotRadius;
			} {
				vw / pw * plotRadius;
			};
			// center of the view, offset by the title space
			prPlotCen = Point(cen.x, max(cen.y, titleOffset + (minDim/2)));
			// update plot center to be the origin of the plot, which
			// may not be in the center of the plot's bounding rect
			originOffset = (
				(plotOrigin.x * prPlotRad / 2) @
				(plotOrigin.y * prPlotRad.neg / 2)
			);
			prPlotCen = prPlotCen - originOffset;

			this.drawInThisOrder;
		};
	}


	drawInThisOrder {
		if (grid.p.show) { grid.stroke };
		thetas !? { // make sure there's data
			if (plots.p.show) { plots.stroke };
			if (legend.p.show) { legend.fill; legend.stroke; };
		};
		if (title.p.show) { title.fill; title.stroke; };
	}


	data_ { |dataArray, thetaArray, units = \scalar, bipolar, refresh = true|
		var shapetest, dataSizes;
		var thetaEnv, dataStep;

		// support 2D arrays: force into shape [1, datasize], i.e. [[1,2,3],[1,3,7]]
		if (dataArray.shape.size == 1) {
			dataArray = [dataArray];
		};

		scalarData = if (units == \db) { dataArray.dbamp } { dataArray };

		if (thetaArray.shape.size == 1) {
			thetaArray = [thetaArray];
		};

		thetas = Array.newClear(dataArray.size);

		dataArray.do{ |dArr, i|
			var thArr = thetaArray.wrapAt(i);
			if (dArr.size != thArr.size) {
				// thetaArray assumed to be breakpoints (likely start/end points)
				// create an "envelope" from breakpoints and generate theta values
				// for each rho value
				dataStep = (dArr.size - 1).reciprocal;
				thetaEnv = Env(thArr, (thArr.size-1).reciprocal, \lin);
				thetas[i] = dArr.collect{ |val, j| thetaEnv.at(dataStep*j) };
			};
		};

		// set dataScalarMin/Max based on bipolar flag
		this.bipolar_(bipolar ?? { this.bipolar }, refresh);
	}

	prCalcDataBounds {
		var flatData;

		flatData = scalarData.flat;
		if (bipolar) { flatData = flatData.abs };
		dataScalarMin = flatData.minItem;
		dataScalarMax = flatData.maxItem;
	}

	bipolar_ { |bool, refresh = true|
		bipolar = bool;
		this.prCalcDataBounds;
		// re-calc plotData based on min/max
		switch (plotUnits,
			\scalar, {
				this.rhoMinMax_(
					if (bipolar) { 0 } { rhoMin },
					rhoMax,
					refresh
				)
			},
			\db, {
				this.rhoMinMax_(
					if (bipolar) { max(clipDbLow, rhoMin) } { rhoMin },
					rhoMax,
					refresh
				)
			}
		)
	}

	// preferred way of setting min and max together
	// min and max should be set in plotUnits
	rhoMinMax_ { |min, max, refresh = true|
		this.rhoMin_(min, false, false);
		this.rhoMax_(max, true, refresh); // rescale data and refresh
	}

	// min should be set in plotUnits
	// use rhoMinMax_ is setting both min and max
	rhoMin_ { |min, rescaleNow = true, refresh = true|
		var rmin, spec;

		rmin = if (bipolar) {
			0
		} {
			if (min == \auto or: { min.isNil }) {
				rhoMin = nil;
				if (plotUnits == \db) {
					max(dataScalarMin.ampdb, clipDbLow)
				} {
					dataScalarMin
				}
			} {
				rhoMin = min;
				min
			}
		};

		spec = if (rhoSpec.notNil) {
			ControlSpec.newFrom(rhoSpec).minval_(rmin)
		} {
			[rmin, rhoMax ?? dataScalarMax].asSpec;
		};

		this.rhoSpec_(spec, rescaleNow, refresh);

		this.rhoGridLinesAt_(
			this.rhoGridLines ?? {
				this.prGetRhoGridLinesFromSpacing(rhoSpec.range / 4, \min, \max)
			},
			false
		);

		rescaleNow.if{ this.prRescalePlotData(refresh) };
	}

	// max should be set in the plotUnits
	// use rhoMinMax_ is setting both min and max
	rhoMax_ { |max, rescaleNow = true, refresh = true|
		var rmax, spec;

		rmax = if (max == \auto or: { max.isNil }) {
			rhoMax = nil;
			if (plotUnits == \db) {
				dataScalarMax.ampdb
			} {
				dataScalarMax
			}
		} {
			rhoMax = max;
			max
		};

		spec = if (rhoSpec.notNil) {
			ControlSpec.newFrom(rhoSpec).maxval_(rmax)
		} {
			[rhoMin ?? dataScalarMin, rmax].asSpec
		};

		this.rhoSpec_(spec, rescaleNow, refresh);

		this.rhoGridLinesAt_(
			this.rhoGridLines ?? {
				this.prGetRhoGridLinesFromSpacing(rhoSpec.range / 4, \min, \max)
			},
			false
		);

		rescaleNow.if{ this.prRescalePlotData(refresh) };
	}


	// plotMin/Max are what's actually used in the plot
	// variables rhoMin/Max can be nil or \auto
	plotMin { ^rhoSpec.minval }
	plotMax { ^rhoSpec.maxval }

	// \db or \scalar
	plotUnits_ { |dbOrScalar, min, max|
		var testMin, newMin, newMax;
		if (plotUnits != dbOrScalar) {
			plotUnits = dbOrScalar;

			if (plotUnits == \db) { // switched to \db scaling
				newMin = min ?? {
					if (rhoSpec.minval.isNegative) {
						warn(
							format("Minimum plot value is negative [%], which can't be converted to dB. \n"
							"Consider changing rhoMin, or first convert use bipolar mode if appropriate for you data.",
								rhoSpec.minval
							)
							);
						plotUnits = \scalar;
						^this
					};
					max(rhoSpec.minval.ampdb, clipDbLow)
				};
				newMax = max ?? rhoSpec.maxval.ampdb;
			} { // switched to \scalar scaling from \db
				newMin = min ?? rhoSpec.minval.dbamp;
				newMax = max ?? rhoSpec.maxval.dbamp;
			};

			// updates plotData
			this.rhoSpec_(
				ControlSpec(newMin, newMax, rhoSpec.warp), // spec always linear, even if data is \db
				true, false // rescale now, don't refresh yet
			);

			// reset the grid lines to new plotUnits
			this.rhoGridLinesAt_(
				if (plotUnits == \db) { rhoGridLines.ampdb } { rhoGridLines.dbamp },
				true  // refresh
			);
		};
	}


	rhoSpec_ { |spec, rescaleNow, refresh=true|
		rhoSpec = spec;
		// TODO: handle switching the plotUnits with the new spec
		rescaleNow.if{ this.prRescalePlotData(refresh: refresh) };
	}


	plotWarp_ { |warp|

		"updating warp not yet implemented".warn;
		// TODO: // rhoSpec.warp_(warp.asWarp);
		// update data, update gridlines
	}

	// called from other methods which set new rhoMin/Max
	prRescalePlotData { |refresh = true|
		var data;

		scalarData ?? { "data has not yet been set".warn; ^this };

		data = if (bipolar) { scalarData.abs } { scalarData };
		plotData = if (plotUnits == \db) {
			// scalarData is scalar, spec is in db, so convert unmap
			rhoSpec.copy.unmap(data.ampdb);
		} {
			rhoSpec.copy.unmap(data);
		};

		refresh.if{ this.refresh };
	}


	defineMouseActions {
		// mouseDownAction = { |v, x, y| };
		// mouseMoveAction = { |v, x, y| };
	}


	plotRadius_ { |normRadius|
		plotRadius = normRadius;
		this.refresh;
	}


	thetaDirection_ { |direction=\cw, refresh = true|
		thetaDirection = direction;
		dirFlag = switch (thetaDirection, \cw, {1}, \ccw, {-1});
		this.thetaZeroPosition_(thetaZeroPosition, false);  // updates prZeroPos, thetaMin
		this.thetaRange_(thetaRange, false);
		refresh.if{ this.refresh };
	}

	// This serves as the reference for start angle
	// When setting this value, reference 0 up, thetaDirection \cw
	thetaZeroPosition_ { |radiansOrPosition = \top, refresh = true|
		thetaZeroPosition = if (radiansOrPosition.isKindOf(Number)) {
			radiansOrPosition
		} {
			switch(radiansOrPosition,
				\top, {0}, \bottom, {pi}, \left, {-0.5pi}, \right, {0.5pi}
			)
		};
		prZeroPos = -0.5pi + thetaZeroPosition; // (thetaZeroPosition * dirFlag);
		this.thetaMin_(thetaMin, false);
		refresh.if{ this.refresh };
	}

	// thetaMin is from thetaZeroPosition, advancing in thetaDirection
	thetaMin_ {|radians=0, refresh = true|
		thetaMin = radians;
		prStartAngle = prZeroPos + (thetaMin*dirFlag);

		if (thetaGridLineSpacing.notNil) {
			this.thetaGridLines_(thetaGridLineSpacing, refresh)
		} {
			this.thetaGridLinesAt_(thetaGridLines, refresh)
		};
	}


	thetaRange_ { |radians=2pi, refresh = true|
		thetaRange = radians;
		prThetaRange = thetaRange * dirFlag;

		if (thetaGridLineSpacing.notNil) {
			this.thetaGridLines_(thetaGridLineSpacing, refresh)
		} {
			this.thetaGridLinesAt_(thetaGridLines, refresh)
		};
	}

	// in plotUnits, grid lines created from "from",
	// stepping "spacing", until reaching "until"
	rhoGridLines_ { |spacing, from = \min, to = \max, refresh = true|
		var lines = this.prGetRhoGridLinesFromSpacing(spacing, from, to);
		this.rhoGridLinesAt_(lines, refresh);
	}

	prGetRhoGridLinesFromSpacing { |spacing, from = \min, to = \max|
		var f, t, step;
		f = switch(from,
			\max, { this.plotMax },
			\min, { this.plotMin },
			{ from }
		);
		t = switch(to,
			\max, { this.plotMax },
			\min, { this.plotMin },
			{ to }
		);
		step = if (f < t) { spacing } { spacing.neg };
		^(f, f+step .. t)
	}

	// an array of levels for the grid lines, in plotUnits
	rhoGridLinesAt_ { |levelArray, refresh|
		rhoGridLines = levelArray.select{ |level|
			level.inRange(this.plotMin, this.plotMax) // clip to rhoMin/Max
		};

		rhoGridLinesNorm = rhoSpec.copy.unmap(rhoGridLines); // TODO: why is .copy required? it fails without it, bug?

		refresh.if{ this.refresh };
	}

	// NOTE: spacing in radians (currently)
	thetaGridLines_ { |spacing, refresh = true|
		thetaGridLines = (thetaMin, thetaMin + spacing .. thetaMin + thetaRange);
		// avoid duplicate lines at 2pi apart
		if ((thetaGridLines.first % 2pi) == (thetaGridLines.last % 2pi), {
			thetaGridLines = thetaGridLines.keep(thetaGridLines.size-1)
		});

		thetaGridLineSpacing = spacing;
		this.prThetaGridLinesAt_(thetaGridLines, refresh);
	}

	// an array of thetaPositions
	thetaGridLinesAt_ { |thetaArray, refresh = true|
		thetaGridLineSpacing = nil;
		this.prThetaGridLinesAt_(thetaArray, refresh);
	}


	prThetaGridLinesAt_ { |thetaArray, refresh = true|
		thetaGridLines = thetaArray.select{ |theta|
			// clip to range of plot
			theta.inRange(thetaMin, thetaMin + thetaRange)
		};
		prThetaGridLines = prZeroPos + (thetaGridLines * dirFlag);
		refresh.if{ this.refresh };
	}


	background_ { |color| userView.background_(color) }
	background { ^userView.background }

	// return bool whether an angle lies within the sweep range
	prAngInRange { |ang|
		var end, newstart, newend;
		var st, sweep;

		st = prStartAngle;
		sweep = prThetaRange;

		if (sweep.isNegative) {
			end = st + sweep;
			newstart = end.wrap(0, 2pi);
			newend = newstart + sweep.abs;
		} {
			newstart = st.wrap(0, 2pi);
			newend = newstart + sweep;
		};
		ang = ang.wrap(0, 2pi);
		// [newstart, newend, ang].round(1e-4).postln;

		^(ang.inRange(newstart, newend) or: {(ang + 2pi).inRange(newstart, newend)});
	}

	// calculate the bounding rect around the plot
	// radius is 1, so coords are top 0@1, bottom 0@-1, right 1@0, left -1@0
	prPlotBounds {
		var st, end, minAng, maxAng, xs, ys;
		var minx, miny, maxx, maxy, width, height;
		var pnts = [];

		st = prStartAngle;
		end = (prStartAngle + prThetaRange).wrap(2pi.neg, 2pi);
		minAng = min(st, end);
		maxAng = max(st, end);
		// right
		if (this.prAngInRange(0)) {
			pnts = pnts.add(1 @ 0)
		};
		// down
		if (this.prAngInRange(0.5pi)) {
			pnts = pnts.add(0 @ -1)
		};
		// up
		if (this.prAngInRange(-0.5pi)) {
			pnts = pnts.add(0 @ 1)
		};
		// left
		if (this.prAngInRange(pi)) {
			pnts = pnts.add(-1 @ 0)
		};
		// zero
		if (this.prAngInRange(prZeroPos)) {
			pnts = pnts.add(Point(cos(prZeroPos), sin(prZeroPos).neg));
		};
		// start
		pnts = pnts.add(Point(cos(st), sin(st).neg));
		// end
		pnts = pnts.add(Point(cos(end), sin(end).neg));
		// origin
		pnts = pnts.add(Point(0, 0));

		xs = pnts.collect(_.x);
		ys = pnts.collect(_.y);
		minx = xs.minItem;
		maxx = xs.maxItem;
		miny = ys.minItem;
		maxy = ys.maxItem;
		width = maxx - minx;
		height = maxy - miny;
		// [minx, maxx, miny, maxy, width, height].postln;

		// origin of this rect describes location of plot origin (center)
		// within the plot bounding rect, from bottom left
		^Rect(
			maxx + minx,  // diff between min and max x
			miny + maxy,  // diff between min and max y
			width, height
		);
	}
}


PolarPlotLayer : ValueViewLayer {
	*properties {
		^(
			show:        true,
			fill:        false,        // fill area under the plotted data
			plotColors:  [Color.blue],
			fillAlpha:   0.3,          // if fill == true, fill the plotColor with this alpha
			strokeWidth: 2,
			// NOTE: for some reason, pointRad won't work if listed after 'strokeType': name conflict with strokeType?
			pointRad:    2,            // if strokeType == \points, circles have pointRad radius. if nil, pointRad == strokeWidth
			strokeTypes: [\line],      // or: \points, or FloatArray.with(5, 2, 3, 2); (dashed alternating 5px, 3px lines separated by 2px)
			negativeColors: [0.09]     // Colors or floats which create a hue shift from the plotColor
		)
	}

	stroke {
		var vZeroPos, vMaxRad, vThetas, sTypes, vScalarData, pntCnt;

		vZeroPos = view.prZeroPos;
		vMaxRad = view.prPlotRad;
		vThetas = view.thetas * view.dirFlag;

		Pen.push;
		Pen.translate(view.prPlotCen.x, view.prPlotCen.y);

		// make sure a single color is held in an array
		if (p.plotColors.isKindOf(Color)) {
			p.plotColors = [p.plotColors]
		};
		if (p.strokeTypes.isKindOf(FloatArray)) {
			// be sure a FloatArray is wrapped into an Array
			p.strokeTypes = [p.strokeTypes]
		};
		sTypes = p.strokeTypes.asArray;


		if (view.bipolar) {
			// prep the data: split source data into positive and
			// negative clumps to reference the sign of the data
			vScalarData = view.scalarData.collect({ |chanData|
				chanData.separate{ |a, b| a.isPositive.xor(b.isPositive) }
			});
		};

		view.plotData.do{ |dataset, i|
			var sType, pnts, thisColor, negColor, strokeLine, dataColors;

			pntCnt = 0;

			// Pen.push;
			// generate data point coordinates
			pnts = dataset.collect{ |val, j|
				Polar(
					val * vMaxRad,
					// wrapAt in case number of theta arrays is mismatched with dataArrays
					// e.g. one theta array is provided, like [0,2pi], for multiple datasets
					vZeroPos + vThetas.wrapAt(i)[j]
				).asPoint;
			};

			sType = sTypes.wrapAt(i);
			strokeLine = (sType != \points);
			if (sType.isKindOf(FloatArray)) {
				Pen.lineDash_(sType)
			};

			Pen.moveTo(pnts[0]);
			Pen.width_(p.strokeWidth);

			if (view.bipolar) {

				/* bipolar = true, data reflects off 0 */

				// create data structure to store colors for data segments
				// for both fill and \points
				dataColors = Array.newClear(vScalarData[i].size);

				if (p.fill or: { strokeLine }) { // pen path needed

					vScalarData[i].do{ |clumpedData, j|
						clumpedData.do{
							Pen.lineTo(pnts[pntCnt]);
							pntCnt = pntCnt + 1;
						};

						// make data pass through zero if another segment follows
						if (j < (vScalarData[i].size - 1)) {
							Pen.lineTo(0@0);
						};

						// choose color of this segment based on sign of data clump
						thisColor = this.prGetDataColor(clumpedData[0].sign, i);
						Pen.strokeColor_(thisColor);

						if (p.fill) {
							dataColors[j] = thisColor; // store in case needed by \points below

							Pen.fillColor_(thisColor.copy.alpha_(thisColor.alpha * p.fillAlpha));
							if (strokeLine) {
								Pen.fillStroke;
							} {
								Pen.fill;
							}
						} {
							Pen.stroke
						};
					};
				};

				if (strokeLine.not) { // stroke == \points
					var rad, rect, thisColor;
					pntCnt = 0;
					rad = p.pointRad ?? p.strokeWidth;
					rect = [0,0,rad*2, rad*2].asRect;  // data point circle

					vScalarData[i].do{ |clumpedData, j|
						thisColor = if (p.fill) {
							dataColors[j]
						} {
							this.prGetDataColor(clumpedData[0].sign, i);
						};
						Pen.strokeColor_(thisColor);

						clumpedData.do{
							Pen.addOval(rect.center_(pnts[pntCnt]));
							pntCnt = pntCnt + 1;
						};
						Pen.stroke;
					}
				}
			} {
				/* bipolar = false */

				thisColor = p.plotColors.asArray.wrapAt(i);
				Pen.strokeColor_(thisColor);

				if (p.fill or: { strokeLine }) {
					// create Pen path
					pnts.do{ |pnt| Pen.lineTo(pnt) };

					if (p.fill) {
						Pen.fillColor_(
							thisColor.copy.alpha_(thisColor.alpha * p.fillAlpha)
						);
						if (strokeLine) {
							Pen.fillStroke;
						} {
							Pen.fill;
						}
					} {
						Pen.stroke;
					};
				};

				if (strokeLine.not) { // stroke == \points
					var rad, rect;
					rad = p.pointRad ?? p.strokeWidth;
					rect = [0,0,rad*2, rad*2].asRect;  // data point circle
					pnts.do{ |pnt| Pen.addOval(rect.center_(pnt)) };
					Pen.stroke;
				}
			};
		};
		Pen.pop;
	}

	fill {}

	prGetDataColor { |sign, chanIdx|
		var negColor, posColor;

		^if (sign.isPositive) {
			p.plotColors.asArray.wrapAt(chanIdx)
		} {
			// negative values
			negColor = p.negativeColors.asArray.wrapAt(chanIdx);

			if (negColor.isKindOf(Color)) {
				negColor
			} {
				// negColor is a number, create a Color
				// with value shift from plotColor
				posColor = p.plotColors.asArray.wrapAt(chanIdx);
				// Color.hsv(*(posColor.asHSV[2] = posColor.asHSV[2] * negColor));
				Color.hsv(*(posColor.asHSV[0] = (posColor.asHSV[0] + negColor).wrap(0, 0.999)));
			};
		};
	}

}

PolarBackgroundLayer  : ValueViewLayer {

	*properties {
		^(
			show:      true,
			fillColor: Color.white,
		)
	}

	fill {
		Pen.push;
		Pen.fillColor_(p.fillColor);
		Pen.addAnnularWedge(view.prPlotCen, 0.001, view.prPlotRad, view.prStartAngle, view.prThetaRange);
		Pen.fill;
		Pen.pop;
	}
}

PolarGridLayer : ValueViewLayer {
	*properties {
		^(
			show:         true,
			fill:         true,
			fillColor:    Color.white,
			showLonLines: true,
			showLatLines: true,
			strokeColor:  Color.gray.alpha_(0.4), // grid color
			showLonVals:  true,
			showLatVals:  true,
			latTxtColor:  Color.gray.alpha_(0.3),
			lonTxtColor:  Color.gray,
			latTxtRound:  0.01,
			lonTxtRound:  1,
			lonTxtWrap:   [0, 2pi], // wrap the grid's longitude text labels around these, in radians
			lonTxtUnits:  \degrees, // or \radians, or \pi
			latTxtAng:    0,        // radian angle of latitude labels, relative to thetaZeroPosition
			lonTxtOffset: 0.065,    // percentage of the radius
			strokeWidth:  1,
		)
	}

	stroke {
		var rad = view.prPlotRad;

		Pen.push;

		// background fill of grid
		if (p.fill) {
			Pen.fillColor_(p.fillColor);
			Pen.addAnnularWedge(view.prPlotCen, 0.001, view.prPlotRad, view.prStartAngle, view.prThetaRange);
			Pen.fill;
		};

		/* lattitude lines */
		Pen.strokeColor_(p.strokeColor);
		Pen.width_(p.strokeWidth);

		if (p.showLatLines) {
			view.rhoGridLinesNorm.do{ |level|
				Pen.addArc(view.prPlotCen, level * rad, view.prStartAngle, view.prThetaRange);
			};
			Pen.stroke;
		};

		if (p.showLonLines) {
			Pen.push;
			Pen.translate(view.prPlotCen.x, view.prPlotCen.y);

			view.prThetaGridLines.do{ |theta|
				Pen.moveTo(0@0);
				Pen.lineTo(Polar(rad, theta).asPoint);
			};
			Pen.stroke;
			Pen.pop;
		};

		/* lattitude labels */
		if (p.showLatVals) {
			var str, theta, strBnds, corner, thetaMod;

			Pen.push;
			Pen.translate(view.prPlotCen.x, view.prPlotCen.y);

			theta = view.prZeroPos + (p.latTxtAng * view.dirFlag);
			thetaMod = theta % 2pi;

			view.rhoGridLinesNorm.do{ |level, i|
				corner = Polar(level * rad, theta).asPoint;
				str = view.rhoGridLines[i].round(p.latTxtRound).asString;
				strBnds = (str.bounds.asArray + [0,0,2,2]).asRect;

				if (thetaMod.inRange(0,pi),
					{ strBnds.bottom_(corner.y) },
					{ strBnds.top_(corner.y) }
				);

				if ((thetaMod < 0.5pi) or: (thetaMod > 1.5pi),
					{ strBnds.right_(corner.x) },
					{ strBnds.left_(corner.x) }
				);

				Pen.stringCenteredIn(str, strBnds, Font("Helvetica", 12), p.latTxtColor);
			};
			Pen.pop;
		};
		Pen.pop;

		/* longitude labels */
		if (p.showLonVals) {
			var str, strVal, theta, strBnds, txtCen, thetaMod, wrapBnds, rnd;

			Pen.push;
			Pen.translate(view.prPlotCen.x, view.prPlotCen.y);

			wrapBnds = p.lonTxtWrap * 1.001; // prevent edge line labels from wrapping, e.g. 45.degrad.wrap(*[-45, 45].degrad)
			rnd = p.lonTxtRound;

			view.prThetaGridLines.do{ |theta, i|
				thetaMod = theta % 2pi;

				txtCen = Polar(rad*(1+p.lonTxtOffset), theta).asPoint;
				str = switch(p.lonTxtUnits,
					\degrees, {
						strVal = view.thetaGridLines[i].wrap(*wrapBnds).raddeg.round(rnd);
						if (strVal % 1 == 0, {strVal.asInt}, {strVal}).asString;
					},
					\radians, {
						strVal = view.thetaGridLines[i].wrap(*wrapBnds).round(rnd);
						if (strVal % 1 == 0, {strVal.asInt}, {strVal}).asString;
					},
					\pi, {
						(view.thetaGridLines[i].wrap(*wrapBnds) / pi).round(rnd) + "π";
					}
				);
				strBnds = (str.bounds.asArray + [0,0,2,2]).asRect;
				strBnds = strBnds.center_(txtCen);

				Pen.stringCenteredIn(str, strBnds, Font("Helvetica", 12), p.lonTxtColor);
			};
			Pen.pop;
		};
		Pen.pop;
	}
}

PolarLegendLayer : ValueViewLayer {
	var nElem, txtRects, lRect, labels, font;
	var updated = false;

	*properties {
		^(
			show:        true,
			fillColor:   Color.white,
			txtColor:    Color.gray,
			align:       \bottomRight, // right, left, top, bottom, topRight, topLeft, bottomRight, bottomLeft
			inset:       10,           // inset between legend and view's edge
			margin:      10,           // inset margin between entries and border of the legend
			spacing:     8,            // spacing between legend entries
			layout:      \vertical,    // horizontal, vertical
			lineLength:  15,           // length of sample plot line
			lineSpacing: 6,            // spacing between sample line and text
			font:        Font("Helvetica"),
			fontSize:    14,
			labels:      [],
			showBorder:  true,
			borderColor: Color.gray,
			borderWidth: 1,
		)
	}

	stroke {
		var lineCols, cursor, h_2, stx, stcursor;
		var pntRad, strokeTypes, cRect;
		var cStep, numcSteps, cOff; // for strokeTupe == \points

		block { |break|
			lRect ?? {break.()}; // bail if background hasn't been calculated

			Pen.push;
			Pen.translate(view.cen.x, view.cen.y); // set 0,0 to center

			if (p.showBorder) {
				Pen.width_(p.borderWidth);
				Pen.strokeColor_(p.borderColor);
				Pen.addRect(lRect);
				Pen.stroke;
			};

			cursor = lRect.leftTop + (p.margin@p.margin);
			// translate to top left of legend background rect
			Pen.translate(cursor.x, cursor.y);
			cursor = 0@0; // reset cursor

			Pen.width = view.plots.strokeWidth;
			lineCols = view.plots.plotColors.asArray;
			strokeTypes = view.plots.p.strokeTypes.asArray;
			pntRad = view.plots.pointRad;
			// h_2 = txtRects[0].height/2;
			cRect = [0,0,pntRad*2,pntRad*2].asRect;

			if (strokeTypes.any(_ == \points)) {
				cStep = cRect.width * 3;  // separation between pnts = pnt diam * 5
				numcSteps = (p.lineLength / cStep).asInt; // how many pnts on the line
				cOff = if (numcSteps > 0) {
					p.lineLength - (cStep * numcSteps) / 2;
				} {
					p.lineLength / 2
				}
			};

			Pen.push;
			nElem.do{ |i|
				var nloops;
				if (view.bipolar) { // bipolar draws both pos and neg color lines
					nloops = 2;
					h_2 = txtRects[0].height/3;
				} {
					nloops = 1;
					h_2 = txtRects[0].height/2;
				};

				// Pen.strokeColor_(lineCols.wrapAt(i));

				if (strokeTypes.wrapAt(i).isKindOf(FloatArray)) {
					Pen.lineDash_(strokeTypes.wrapAt(i))
				};

				switch(p.layout,
					\horizontal, {
						stcursor = cursor.copy;

						nloops.do { |j| // loop twice in the case of two lines for bipolar data
							h_2 = h_2 * (j+1);
							Pen.strokeColor_( view.plots.prGetDataColor(1 - (2*j), i));
							cursor = stcursor;

							// w = margin-lineLength-lineSpacing-text length-spacing-lineLength-lineSpacing-text length-margin, etc
							if (strokeTypes.wrapAt(i) == \points) {
								cursor = cursor + (cOff@h_2);
								Pen.addOval(cRect.center_(cursor));
								numcSteps.do {
									cursor = cursor + (cStep@0);
									Pen.addOval(cRect.center_(cursor));
								};
								cursor = cursor + ((cOff+p.lineSpacing)@h_2.neg);
							} { // lines or dashes
								cursor = cursor + (0@h_2);
								Pen.moveTo(cursor);
								cursor = cursor + (p.lineLength@0);
								Pen.lineTo(cursor);
								cursor = cursor + (p.lineSpacing@h_2.neg);
							};
							Pen.stroke;
						};

						Pen.stringLeftJustIn(
							labels[i], txtRects[i].left_(cursor.x).top_(cursor.y),
							font, p.txtColor
						);
						cursor.x = cursor.x + txtRects[i].width + p.spacing;
					},
					\vertical, {
						stcursor = cursor.copy;

						nloops.do { |j| // loop twice in the case of two lines for bipolar data
							h_2 = h_2 * (j+1);
							Pen.strokeColor_(view.plots.prGetDataColor(1 - (2*j), i));
							cursor = stcursor;

							// h = margin-txtHeight-spacing-txtHeight-margin
							if (strokeTypes.wrapAt(i) == \points) {
								cursor = cursor + (cOff@h_2);
								Pen.addOval(cRect.center_(cursor));
								numcSteps.do {
									cursor = cursor + (cStep@0); // step
									Pen.addOval(cRect.center_(cursor));
								};
								cursor = cursor + ((cOff+p.lineSpacing)@h_2.neg);
							} { // lines or dashes
								cursor = cursor + (0@h_2);
								Pen.moveTo(cursor);
								cursor = cursor + (p.lineLength@0);
								Pen.lineTo(cursor);
								cursor = cursor + (p.lineSpacing@h_2.neg);
							};
							Pen.stroke;
						};

						Pen.stringLeftJustIn(
							labels[i], txtRects[i].left_(cursor.x).top_(cursor.y),
							font, p.txtColor
						);
						cursor.x = stcursor.x; // jump back to starting x
						cursor.y = cursor.y + txtRects[0].height + p.spacing;
					}
				);
			};
			Pen.pop;
			Pen.pop;
		};
	}

	fill {
		var sumW, sumH, spacing, margin;

		nElem = view.plotData.size;
		if (nElem > 0) {
			Pen.push;
			Pen.translate(view.cen.x, view.cen.y);

			if (p.labels.isKindOf(String)) {
				p.labels = [p.labels]
			};

			labels = if (p.labels.size == 0) {
				view.plotData.size.collect{|i| format("Plot %", i+1) }
			} { // make sure there are labels for all plots
				p.labels.asArray.extend(nElem, " - ");
			};

			font = p.font.size_(p.fontSize);
			txtRects = labels.collect(_.bounds(font));
			spacing = p.spacing;
			margin = p.margin;

			switch(p.layout,
				\horizontal, {
					// w = margin-lineLength-lineSpacing-textlength-spacing-lineLength-lineSpacing-text length-spacing, etc, - margin
					sumW = margin + (p.lineLength + p.lineSpacing + spacing * nElem) + txtRects.collect(_.width).sum - spacing + margin;
					sumH = (margin * 2) + txtRects[0].height;
				},
				\vertical, {
					// h = spacing-txtHeight-spacing-txtHeight-spacing
					sumW = margin + p.lineLength + p.lineSpacing + txtRects.collect(_.width).maxItem + margin;
					sumH = margin + (txtRects[0].height + spacing * nElem) - spacing + margin;
				}
			);

			lRect = [0,0, sumW, sumH].asRect.center_(0@0);

			switch(p.align,
				\right,  {lRect.right = view.bnds.width/2 - p.inset},
				\left,   {lRect.left = view.bnds.width/2.neg + p.inset},
				\top,    {lRect.top = view.bnds.height/2.neg + p.inset},
				\bottom, {lRect.bottom = view.bnds.height/2 - p.inset},
				\topRight, {
					lRect.top = view.bnds.height/2.neg + p.inset;
					lRect.right = view.bnds.width/2 - p.inset;
				},
				\topLeft, {
					lRect.top = view.bnds.height/2.neg + p.inset;
					lRect.left = view.bnds.width/2.neg + p.inset;
				},
				\bottomRight, {
					lRect.bottom = view.bnds.height/2 - p.inset;
					lRect.right = view.bnds.width/2 - p.inset;
				},
				\bottomLeft, {
					lRect.bottom = view.bnds.height/2 - p.inset;
					lRect.left = view.bnds.width/2.neg + p.inset;
				},
			);

			Pen.fillColor_(p.fillColor);
			Pen.fillRect(lRect);
			Pen.fill;
			Pen.pop;
		}
	}
}

PolarTitleLayer : ValueViewLayer {
	var font, txtRect, <bgRect, bndCalcd;

	*properties {
		^(
			show:        true,
			fillColor:   Color.white,
			txtColor:    Color.gray,
			align:       \top,
			inset:       10,    // pixel distance inset from the top of the view
			margin:      15,    // margin around text and title border
			txt:         "plot",
			font:        Font("Helvetica"),
			fontSize:    18,
			showBorder:  true,
			borderColor: Color.gray,
			borderWidth: 1,
		)
	}

	// fill needs to be called before stroke
	stroke {
		Pen.push;
		if (p.showBorder) {
			Pen.width_(p.borderWidth);
			Pen.strokeColor_(p.borderColor);
			Pen.addRect(bgRect);
			Pen.stroke;
		};
		Pen.stringCenteredIn(p.txt, txtRect, font, p.txtColor);
		Pen.stroke;
		Pen.pop;
	}

	fill {
		if (bndCalcd.not) {
			this.calcBounds;
			bndCalcd = false
		};
		Pen.push;
		Pen.fillColor_(p.fillColor);
		Pen.fillRect(bgRect);
		Pen.fill;
		Pen.pop;
	}

	calcBounds {
		font = p.font.size_(p.fontSize);
		// String:-bounds doesn't account for newlines, so add it up
		txtRect = if (p.txt.contains("\n")) {
			var rects, w, h;
			rects = p.txt.split($\n).collect(_.bounds(font));
			w = rects.collect(_.width).maxItem + 4; // give a lil extra width
			h = rects[0].height * rects.size;
			[0,0,w,h].asRect
		} {
			p.txt.bounds(font)
		};
		bgRect = txtRect + [0, 0, p.margin, p.margin].asRect;
		bgRect = bgRect.center_(view.bnds.width/2 @ (bgRect.height/2 + p.inset));
		txtRect = txtRect.center_(bgRect.center);
		bndCalcd = true;
	}

}

/*

(
v = PolarView(bounds: Size(500, 500).asRect, specs: [[0,2pi].asSpec],
	initVals: 30.collect{rrand(-2pi,2pi)},
	thetaDirection: \ccw,
	thetaZeroPosition: \right,
	plotUnits: \scalar
).front;
v.title.txt_("Two plots: sine and cosine");
v.data_([sinPi((0,0.005 .. 1)), cosPi((0,0.005 .. 1))], [0, 2pi], \scalar); // sine window
v.plots.plotColors = 2.collect{Color.rand};
v.legend.labels_(["sine", "cosine"]);
)

// 2 plots of data
v.plots.fill = true
v.plots.fillAlpha = 0.3

v.rhoGridLines_(1, 0.25, -1) // note -1 is the minimum, in the center
v.plots.strokeType = \points
v.plots.pointRad = 1
v.plots.strokeType = [\line,\points] // different stroke styles

v.plots.strokeType = [\points, FloatArray.with(*[6, 3, 2, 3])]
v.legend.lineLength = 25 // make lineLength long enough to see the dashed pattern
v.legend.align
v.legend.layout = \horizontal

// switch to a dB scaling
v.plotUnits = \db
v.rhoMin_(-40)
v.rhoGridLines_(0, 5, -40)


*/