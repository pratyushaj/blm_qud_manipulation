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
      $(".err").hide();
      this.init_sliders(); 
      //var checked_radio  = $('input[name="response"]:checked');     
      exp.sliderPost = null;
      //checked_radio.val() = undefined;
      // $('input[name="sense"]:checked').attr('checked',false);
      this.stim = stim; //FRED: allows you to access stim in helpers
      //var noun_data = _.sample(corpus.Noun)
      //this.noun_data = noun_data;
      //var noun = noun_data.noun;
      //var animacy = noun_data.animacy;
      $("input[type=radio]").attr("checked", null);

      this.verbs = _.shuffle(["is","is not"])

      var names_list = _.shuffle(names);

      var man1 = names_list[0];
      var man2 = names_list[1];
      var pos = stim.POS
      var uppercaseNoun = stim.Noun.charAt(0).toUpperCase() + stim.Noun.slice(1);

      $(".man1").html(man1);

      $(".man2").html(man2);

      $(".noun").html((stim.Noun).toLowerCase());
      $(".predicate").html((stim.Predicate))
      $(".nounclass").html(stim.NounClass)

      //$(".woman1").html(woman1);

      //$(".woman2").html(man2);

      //$(".utterance1").html("\"That "+ stim.Noun + " " + this.verbs[0] + " " + stim.Predicate + ".\"");

      //$(".utterance2").html("\"You're wrong. That "+ stim.Noun + " " + this.verbs[1] + " "  + stim.Predicate + ".\"");

      //Statement 1: Stimulus
      if (stim.POS == 'adjective')
      {
        $(".stimulus").html("<strong>"+uppercaseNoun + " are " + stim.Predicate + ".</strong>");
      }
      else if (stim.POS == 'verb')
      {
        $(".stimulus").html("<strong>"+uppercaseNoun + " " + stim.Predicate + ".</strong>");
      }
      
      //Statement 2: Everyone 
      if (stim.POS == 'adjective')
      {
        $(".everyone").html("Everyone is " + stim.Predicate + ".");
      }
      else if (stim.POS == 'verb')
      {
        if (stim.Predicate == 'lay eggs')
        {
          $(".everyone").html("Everyone lays eggs.");
        }
        else if (stim.Predicate == 'eat carrots'){
          $(".everyone").html("Everyone eats carrots.");
        }
        else if (stim.Predicate == 'solve problems'){
          $(".everyone").html("Everyone solves problems.");
        }
        else if (stim.Predicate == 'stretch'){
          $(".everyone").html("Everyone stretches.");
        }
        else if (stim.Predicate == 'fly'){
          $(".everyone").html("Everyone flies.");
        }
        else{
          $(".everyone").html("Everyone " + stim.Predicate + "s.");
        }
      }

      //Not all
      if (stim.POS == 'adjective')
      {
        $(".notall").html("Not all " + stim.Noun + " are " + stim.Predicate + ".");
      }
      else if (stim.POS == 'verb')
      {
        $(".notall").html("Not all " + stim.Noun + " " + stim.Predicate + ".");
      }


      //No 
      if (stim.POS == 'adjective')
      {
        $(".no").html("No, " + stim.Noun + " are not " + stim.Predicate + ".");
      }
      else if (stim.POS == 'verb')
      {
        $(".no").html("No, " + stim.Noun + " do not " + stim.Predicate + ".");
      }

      //Yes
      if (stim.POS == 'adjective')
      {
        $(".yes").html("Yes, " + stim.Noun + " are " + stim.Predicate + ".");
      }
      else if (stim.POS == 'verb')
      {
        $(".yes").html("Yes, " + stim.Noun + " " + stim.Predicate + ".");
      }


//      this.sentence_types = _.shuffle(["yes","no"]);
//      this.sentence_types = ["no","yes"];
//      var sentences = {
//        "yes": "Yes, it's a matter of opinion.",
//        "no": "No, somebody must be wrong.",
//      };

//      this.n_sliders = this.sentence_types.length;
		  this.n_sliders = 1;
//      $(".slider_row").remove();
//      for (var i=0; i<this.n_sliders; i++) {
//        var sentence_type_left = this.sentence_types[0];
//        var sentence_type_left = this.sentence_types[1];        
//        var sentence_left = sentences[sentence_type_left];
//        var sentence_right = sentences[sentence_type_right];        
//        $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="sentence0">' + "<font size='4'>" + sentence_left + "</font>" + '</td><td colspan="2"><div id="slider0" class="slider">-------[ ]--------</div></td><td class="slider_target" id="sentence1">' + "<font size='4'>" + sentence_right + "</font>" + '</td></tr>');
//        utils.match_row_height("#multi_slider_table", ".slider_target");
//      }

    },


    button : function() {
      var checked_radio  = $('input[name="response"]:checked');
      if (checked_radio.val() != undefined) 
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
        exp.sliderPost = ui.value;
      });

      utils.make_slider("#slider1", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
//    make_slider_callback : function(i) {
//      return function(event, ui) {
//        exp.sliderPost[i] = ui.value;
//      };
//    },
    log_responses : function() {
        exp.data_trials.push({
          "response" : $('input[name="response"]:checked').val(),
          "noun" : this.stim.Noun,          
          "predicate" : this.stim.Predicate,
          "nounclass" : this.stim.NounClass,
          "class" : this.stim.Class,                    
          "firstutterance" : this.verbs[0],
          "block_number" : "1",      
          "slide_number" : exp.phase,
          "block":"production"
        });
    },
  });

  slides.instructions2 = slide({
    name : "instructions2",
    start: function() {
      $(".instruction_condition").html("Between subject instruction manipulation: "+ exp.instruction);
    }, 
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  //put code for the second set of sliders here 
  slides.identity = slide({
    name : "identity",
    present : second_stimuli, //make second set of stimuli here 
    present_handle : function(stim) {
      $(".err").hide();
      this.init_sliders();      
      exp.sliderPost = null;
      // $('input[name="sense"]:checked').attr('checked',false);
      this.stim = stim; //FRED: allows you to access stim in helpers
      //var noun_data = _.sample(corpus.Noun)
      //this.noun_data = noun_data;
      //var noun = noun_data.noun;
      //var animacy = noun_data.animacy;
      //this.verbs = _.shuffle(["is","is not"])
      //var names_list = _.shuffle(names);
      $("input[type=radio]").attr("checked", null);

      //var man1 = names_list[0];
      //var man2 = names_list[1];

      //$(".man1").html(man1);

      //$(".man2").html(man2);

      $(".singular").html(stim.Singular);
      $(".noun").html(stim.Noun);

      //$(".woman1").html(woman1);

      //$(".woman2").html(man2);

      //$(".utterance1").html("\"That "+ stim.Noun + " " + this.verbs[0] + " " + stim.Predicate + ".\"");

      //$(".utterance2").html("\"You're wrong. That "+ stim.Noun + " " + this.verbs[1] + " "  + stim.Predicate + ".\"");

//      this.sentence_types = _.shuffle(["yes","no"]);
//      this.sentence_types = ["no","yes"];
//      var sentences = {
//        "yes": "Yes, it's a matter of opinion.",
//        "no": "No, somebody must be wrong.",
//      };

//      this.n_sliders = this.sentence_types.length;
    this.n_sliders = 1;
//      $(".slider_row").remove();
//      for (var i=0; i<this.n_sliders; i++) {
//        var sentence_type_left = this.sentence_types[0];
//        var sentence_type_left = this.sentence_types[1];        
//        var sentence_left = sentences[sentence_type_left];
//        var sentence_right = sentences[sentence_type_right];        
//        $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="sentence0">' + "<font size='4'>" + sentence_left + "</font>" + '</td><td colspan="2"><div id="slider0" class="slider">-------[ ]--------</div></td><td class="slider_target" id="sentence1">' + "<font size='4'>" + sentence_right + "</font>" + '</td></tr>');
//        utils.match_row_height("#multi_slider_table", ".slider_target");
//      }

    },

    button : function() {
      console.log(exp.sliderPost);
      var checked_radio  = $('input[name="response2"]:checked');
      if (checked_radio.val() != undefined) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },

    init_sliders : function() {
      utils.make_slider("#slider0", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
//    make_slider_callback : function(i) {
//      return function(event, ui) {
//        exp.sliderPost[i] = ui.value;
//      };
//    },
    log_responses : function() {
        exp.data_trials.push({
          "response": $('input[name="response2"]:checked').val(),
          "noun" : this.stim.Noun,          
          "nounclass" : this.stim.NounClass,
          "class" : "NA",                    
          "firstutterance" : "NA",      
          "block_number" : exp.blockidentity,           
          "slide_number" : exp.phase,
          "block":"identity"
        });
    },
});

  slides.likeability = slide({
    name : "likeability",
    present : third_stimuli, //make second set of stimuli here 
    present_handle : function(stim) {
      $(".err").hide();
      this.init_sliders();      
      exp.sliderPost = null;
      // $('input[name="sense"]:checked').attr('checked',false);
      this.stim = stim; //FRED: allows you to access stim in helpers
      //var noun_data = _.sample(corpus.Noun)
      //this.noun_data = noun_data;
      //var noun = noun_data.noun;
      //var animacy = noun_data.animacy;
      //this.verbs = _.shuffle(["is","is not"])
      //var names_list = _.shuffle(names);
      //$("input[type=radio]").attr("checked", null);

      //var man1 = names_list[0];
      //var man2 = names_list[1];

      //$(".man1").html(man1);

      //$(".man2").html(man2);

      $(".singular").html(stim.Singular);
      $(".noun").html(stim.Noun);

      //$(".woman1").html(woman1);

      //$(".woman2").html(man2);

      //$(".utterance1").html("\"That "+ stim.Noun + " " + this.verbs[0] + " " + stim.Predicate + ".\"");

      //$(".utterance2").html("\"You're wrong. That "+ stim.Noun + " " + this.verbs[1] + " "  + stim.Predicate + ".\"");

//      this.sentence_types = _.shuffle(["yes","no"]);
//      this.sentence_types = ["no","yes"];
//      var sentences = {
//        "yes": "Yes, it's a matter of opinion.",
//        "no": "No, somebody must be wrong.",
//      };

//      this.n_sliders = this.sentence_types.length;
    this.n_sliders = 1;
//      $(".slider_row").remove();
//      for (var i=0; i<this.n_sliders; i++) {
//        var sentence_type_left = this.sentence_types[0];
//        var sentence_type_left = this.sentence_types[1];        
//        var sentence_left = sentences[sentence_type_left];
//        var sentence_right = sentences[sentence_type_right];        
//        $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="sentence0">' + "<font size='4'>" + sentence_left + "</font>" + '</td><td colspan="2"><div id="slider0" class="slider">-------[ ]--------</div></td><td class="slider_target" id="sentence1">' + "<font size='4'>" + sentence_right + "</font>" + '</td></tr>');
//        utils.match_row_height("#multi_slider_table", ".slider_target");
//      }

    },

    button : function() {
      console.log(exp.sliderPost);
      //var checked_radio  = $('input[name="response2"]:checked');
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },

    init_sliders : function() {
      utils.make_slider("#slider0", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
//    make_slider_callback : function(i) {
//      return function(event, ui) {
//        exp.sliderPost[i] = ui.value;
//      };
//    },
    log_responses : function() {
        exp.data_trials.push({
          "response" : exp.sliderPost,
          //"radio_response": $('input[name="response2"]:checked').val(),
          "noun" : this.stim.Noun,          
          "nounclass" : this.stim.NounClass,
          "class" : "NA",                    
          "firstutterance" : "NA",
          "block_number" : exp.blocklikeability,           
          "slide_number" : exp.phase,
          "block":"likeability"
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
