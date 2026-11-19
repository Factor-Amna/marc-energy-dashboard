# Click-through station viewer (writes HTML). Needs hist_helpers.R sourced first.
fmt_usd <- function(x) ifelse(is.na(x), "not available", paste0("$", format(round(x), big.mark = ",")))
render_station_viewer <- function() {
facts <- function(r) {
  f <- tibble(
    Item = c("Line", "Weekday ridership", "Parking spots", "Area population (2010)",
             "Intersection density", "Transit score", "Utilization 2014",
             "EV charging"),
    Value = c(as.character(r$line), format(round(r$ridership), big.mark = ","), format(r$parking, big.mark = ","),
              format(r$population, big.mark = ","), format(round(r$density)),
              ifelse(is.na(r$transit_score), "none", r$transit_score),
              ifelse(is.na(r$utilization), "not available", paste0(round(r$utilization), "%")),
              ifelse(r$ev == 1, "Yes", "No")))
  if (!is.na(r$building_sf)) {
    f <- bind_rows(f, tibble(
      Item = c("Building sq ft", "Meters on file", "Lighting and controls, low to high"),
      Value = c(paste0(format(round(r$building_sf), big.mark = ","), " (", r$area_source, ")"),
                as.character(ifelse(is.na(r$meters), "no record", r$meters)),
                paste0(fmt_usd(r$lighting_controls_low), " to ", fmt_usd(r$lighting_controls_high)))))
  }
  f
}
png_uri <- function(p) {
  f <- tempfile(fileext = ".png")
  ggsave(f, p, width = 8, height = 4.4, dpi = 90, bg = bg)
  paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.size(f))))
}
ord <- stations %>% mutate(line = factor(line, c("Penn", "Brunswick", "Camden"))) %>% arrange(line, station)

opts <- paste0('<option value="', seq_len(nrow(ord)) - 1, '">', ord$station, ' (', ord$line, ')</option>', collapse = "")
cards <- vapply(seq_len(nrow(ord)), function(i) {
  r <- ord[i, ]
  tab <- knitr::kable(facts(r), format = "html", col.names = c("", ""), table.attr = 'class="table table-condensed" style="width:auto"')
  paste0('<div class="stn-card" style="display:', ifelse(i == 1, "block", "none"), '">',
         '<h3>', r$station, ' <small>', r$line, '</small></h3>',
         '<div class="stn-row"><div class="stn-facts">', paste(tab, collapse = ""), '</div>',
         '<div class="stn-plot"><img src="', png_uri(plot_station(r)), '" style="width:100%"/>',
         '<p class="stn-note">Red dashed line marks this station, and the gold line marks the median of all 38.</p></div></div></div>')
}, character(1))

cat('<div class="stn-bar">',
    '<button id="stn-prev" class="btn btn-default">&larr; Previous</button>',
    '<select id="stn-select" class="form-control" style="display:inline-block;width:auto;margin:0 8px">', opts, '</select>',
    '<button id="stn-next" class="btn btn-default">Next &rarr;</button>',
    '<span id="stn-count" style="margin-left:12px;color:#6B665E"></span>',
    '<span style="margin-left:18px">Jump to: ',
    '<a href="#" class="stn-mode" data-m="Penn">Penn</a> | <a href="#" class="stn-mode" data-m="Brunswick">Brunswick</a> | <a href="#" class="stn-mode" data-m="Camden">Camden</a></span>',
    '<span style="margin-left:18px;color:#6B665E">Arrow keys also work</span></div>',
    paste(cards, collapse = ""),
    '<script>(function(){',
    'var cards=document.querySelectorAll(".stn-card"),sel=document.getElementById("stn-select"),cnt=document.getElementById("stn-count"),i=0;',
    'var modes=', jsonlite::toJSON(as.character(ord$line)), ';',
    'function show(k){i=(k+cards.length)%cards.length;for(var j=0;j<cards.length;j++)cards[j].style.display="none";cards[i].style.display="block";sel.value=i;cnt.textContent=(i+1)+" / "+cards.length;}',
    'document.getElementById("stn-prev").onclick=function(){show(i-1)};',
    'document.getElementById("stn-next").onclick=function(){show(i+1)};',
    'sel.onchange=function(){show(parseInt(sel.value))};',
    'var ml=document.querySelectorAll(".stn-mode");for(var m=0;m<ml.length;m++){ml[m].onclick=function(e){e.preventDefault();show(modes.indexOf(this.getAttribute("data-m")))}}',
    'document.addEventListener("keydown",function(e){if(e.target.tagName==="SELECT"||cards[0].offsetParent===null)return;if(e.key==="ArrowRight")show(i+1);if(e.key==="ArrowLeft")show(i-1);});',
    'show(0);})();</script>', sep = "\n")
}
