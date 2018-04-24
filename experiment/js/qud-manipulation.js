//THINGS TO DO:
//LEFT ALIGN RADIO BUTTONS 
//MAKE SURE STIMULI ARE CORRECT (IN NUMBER AND VERB AGREEMENT, STIM VERSUS RESPONSE STATEMENTS)



function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions1 = slide({
    name : "instructions1",
    start: function() {
      $(".instruction_condition").html("Between subject instruction manipulation: "+ exp.instruction);
    }, 
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.cover_stories = slide({
    name : "cover_stories",
    present : stimuli,
    present_handle : function(stim) {

      //hiding error message on first presentation
      $(".err").hide();

      //create 2 sliders per trial, both set to null on first presentation
      this.init_sliders(); 
      exp.sliderPost1 = null;
      exp.sliderPost2 = null;
     
      //getting access to stimuli
      this.stim = stim; //FRED: allows you to access stim in helpers
      
      var pos = stim.POS

      $(".crit_noun").html((stim.CritNoun));
      $(".other_noun").html((stim.OtherNoun));
      $(".predicate").html((stim.Predicate));
      $(".opposite").html((stim.Opposite));
      $(".statement").html(stim.Statement);
      $(".cover_story").html(stim.CoverStory);

		  this.n_sliders = 2;

    },


    button : function() {
      if (exp.sliderPost1 != null && exp.sliderPost2 !=null) 
      {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else 
      {
        $(".err").show();
      }
    },

    init_sliders : function() {
      utils.make_slider("#slider0", function(event, ui) {
        exp.sliderPost1 = ui.value;
      });

      utils.make_slider("#slider1", function(event, ui) {
        exp.sliderPost2 = ui.value;
      });
    },
//    make_slider_callback : function(i) {
//      return function(event, ui) {
//        exp.sliderPost[i] = ui.value;
//      };
//    },
    log_responses : function() {
        exp.data_trials.push({
          "response" : exp.sliderPost1,
          "crit_noun" : this.stim.CritNoun,
          "response_other":exp.sliderPost2,
          "other_noun": this.stim.OtherNoun,         
          "predicate" : this.stim.Predicate,
          "nounclass" : this.stim.NounClass,
          "qud":this.stim.QUD,
          "class" : this.stim.Class,                    
          "block_number" : "1",      
          "slide_number" : exp.phase,
          "block":"cover_stories"
        });
    },
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.trials = [];
  exp.catch_trials = [];
  exp.instruction = _.sample(["instruction1","instruction2"]);
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  //shuffle array of block names, put block_names[0] in one and block_names[1] in the other 
  block_names = _.shuffle(['likeability','identity']);
  exp.blocklikeability = 0;
  exp.blockidentity = 0;
  if (block_names[0] == "likeability") {
    exp.blocklikeability = 2;
    exp.blockidentity = 3;
  } else {
    exp.blocklikeability = 3;
    exp.blockidentity = 2;    
  }
  exp.structure=["i0", "instructions1",'cover_stories', 'subj_info', 'thanks'];
  
  exp.data_trials = [];
  //make corresponding slides:

  exp.slides = make_slides(exp);

  exp.nQs = 5+45+9+9;//

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}
