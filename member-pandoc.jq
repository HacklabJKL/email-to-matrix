def pandocify:
{
  "blocks": [
    {
      "t": "Table",
      "c": [
        [],
        [
          {
            "t": "AlignDefault"
          },
          {
            "t": "AlignDefault"
          }
        ],
        [
          0,
          0
        ],
        [
          [],
          []
        ],
        map(
          [
            [
              {
                "t": "Plain",
                "c": [
                  {
                    "t": "Str",
                    "c": .key
                  }
                ]
              }
            ],
            [
              {
                "t": "Plain",
                "c": [
                  {
                    "t": "Str",
                    "c": .value
                  }
                ]
              }
            ]
          ]
        )
      ]
    }
  ],
  "pandoc-api-version": [
    1,
    17,
    5,
    4
  ],
  "meta": {}
}
;

def split_email:
split("\n") | reduce .[] as $item ({"header":[]};
if has("msg") then
    .msg += [$item]
else
    if has("body") then
        if $item == "msg" then
	    .msg |= []
	else
	    .body += [$item]
        end
    else
        if has("msg") then
	    .msg += [$item]
	else
	    if $item == "" then
	        .body |= []
	    else
	        .header += [$item]
	    end
        end
    end
end
)
;

def join_lines:
    map(match("^ *(.*?) *$") | .captures[0].string // "") | map(select(.!="")) | join(" ")
;

split_email | (.body| map(capture("^(?<key>[^ ]*) *(?<value>.*)"))) + [{"key":"msg","value":(.msg | join_lines)}] | pandocify
