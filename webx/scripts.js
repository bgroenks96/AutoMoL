function updateProof(){
		MathJax.Hub.Queue(["Typeset", MathJax.Hub, "top"]);
}
		
// the following adapted from to Farnando Carpani
//https://codepen.io/pkra/pen/VpEqdd?editors=1010
//https://groups.google.com/forum/#!msg/mathjax-users/Do5E5delM9w/wsnDFOvxCgAJ
window.MathJax = {
  jax: ["input/TeX", "input/MathML", "input/AsciiMath", "output/HTML-CSS"],
  extensions: ["tex2jax.js", "asciimath2jax.js", "mml2jax.js", "MathMenu.js", "MathZoom.js"],
  AsciiMath: {useMathMLspacing: true},
  TeX: {
    extensions: ["AMSmath.js", "AMSsymbols.js", "autoload-all.js"],
    Macros: {
          infer: ["\\begin{array}\[b\]{c c c c}"+
          "\\style{border-bottom:1px solid;}{\\begin{array}\[b\]{c c c c}"+
          "#3 \\\\"+
          "\\end{array}} &  \\hspace{-1em}\\raise{-0.25em}{#1} \\\\"+
          "#2"+
          "\\end{array}",3,""
          ],
        inferbasic: [
        	"\\begin{array}\[b\]{c c c c}"+
        	"\\style{border-top:1px solid;}{#2} & \\hspace{-0.75em}\\raise{0.5em}{#1} \\\\"+
        	"\\end{array}",2, ""
        ]
        }
  },
  MathML: {
    extensions: ["mml3.js", "content-mathml.js"]
  },
  tex2jax: {
    inlineMath: [
      ['$', '$'],
      ["\\(", "\\)"]
    ],
    processEscapes: true
  },
  SVG: { font: 'STIX-Web'},
  AuthorInit: function() {
    MathJax.Hub.Register.StartupHook("Begin", function() {
        // your code to run once MathJax is ready, e.g., custom extensions etc.        
      MathJax.Hub.Queue( function(){
        // something to queue after the initial typesetting is done
      }
      );
    });
  }
};

(function(d, script) {
  script = d.createElement('script');
  script.type = 'text/javascript';
  script.async = true;
  script.onload = function() {
    // remote script has loaded
  };
  script.src = 'https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js';
  d.getElementsByTagName('head')[0].appendChild(script);
}(document));