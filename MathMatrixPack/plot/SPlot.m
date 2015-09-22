Style8 = {
	{Hue[0.6, 1, 1], Thickness[0.005]}, 
	{Hue[0.3, 1, 1], Thickness[0.006], Dashed}, 
	{Hue[1, 1, 1], Thickness[0.007], Dotted}, 
	{Hue[0.1, 1, 1], Thickness[0.005]}, 
	{Hue[0.9, 1, 1], Thickness[0.006], Dashed}, 
	{Hue[0.5, 1, 1], Thickness[0.007], Dotted}, 
	{Hue[0.2, 1, 1], Thickness[0.005]}, 
	{Hue[0.8, 1, 1], Thickness[0.006], Dashed}
	};

SPlot[xExpression_, xInterval_, xFrameLabel_ : {}, xLegend_ : {}, xPlotLabel_String : "", xScale_ : 1.15] :=
	Module[{xStyle = Style8}, 
		TableForm[ {
			Plot[
				xExpression, 
				xInterval, 
				PlotStyle -> Style8, 
				PlotRange -> Full, 
				Frame -> True, 
				GridLines -> Automatic,
				ImageSize -> xScale {500, 300},
				FrameLabel -> xFrameLabel, 
				PlotLabel -> xPlotLabel 
				], 
			Graphics[ {
				Black, 
				Directive[FontFamily -> "Arial", FontSize -> 16], 
				MapIndexed[ Text[#1, {10 (First[#2] - 1) + 6, 0}] &, xLegend], 
				MapIndexed[ Join[ 
					Last[xStyle = RotateLeft @ xStyle], 
					{Line[{{10 (First[#2] - 1), 0}, {10 (First[#2] - 1) + 3, 0}}]}
					] &, 
					xLegend
					]
				}, 
			ImageSize -> 1.15 {500, 30}]}
			]
		];