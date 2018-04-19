// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var predicates = _.shuffle([
		{"Predicate":"generous", "Class":"positive","POS":"adjective"},
		{"Predicate":"kind", "Class":"positive","POS":"adjective"}, 
		{"Predicate":"competent", "Class":"positive","POS":"adjective"}, 
		{"Predicate":"strong", "Class":"positive","POS":"adjective"}, 					
		{"Predicate":"honest", "Class":"positive","POS":"adjective"}, 					
		{"Predicate":"smart", "Class":"positive","POS":"adjective"}, 							
		{"Predicate":"unintelligent", "Class":"negative","POS":"adjective"}, 
		{"Predicate":"weak", "Class":"negative","POS":"adjective"}, 
		{"Predicate":"incompetent", "Class":"negative","POS":"adjective"}, 
		{"Predicate":"sexist", "Class":"negative","POS":"adjective"}, 
		{"Predicate":"lie", "Class":"negative","POS":"verb"}, 
		{"Predicate":"rude", "Class":"negative","POS":"adjective"}
]);

var neutrals = _.shuffle([
		{"Predicate":"walk", "Class":"neutral","POS":"verb"}, 						
		{"Predicate":"sleep", "Class":"neutral","POS":"verb"}, 
		{"Predicate":"eat", "Class":"neutral","POS":"verb"}, 						
		{"Predicate":"breathe", "Class":"neutral","POS":"verb"} 
]);

var nouns = [
		{"Noun":"men", "NounClass":"genders","Singular":"man"},
		{"Noun":"women", "NounClass":"genders","Singular":"woman"},
		{"Noun":"Republicans", "NounClass":"political parties","Singular":"Republican"},
		{"Noun":"Democrats", "NounClass":"political parties","Singular":"Democrat"},
		{"Noun":"people in their 20's", "NounClass":"age","Singular":"person in their 20's"},								
		{"Noun":"people in their 50's", "NounClass":"age","Singular":"person in their 50's"},								
		{"Noun":"Americans", "NounClass":"nationalities","Singular":"American"},								
		{"Noun":"Europeans", "NounClass":"nationalities","Singular":"European"},
];

var stimuli =  _.shuffle(makeStims());
//var stimuli = makeStims();

var second_stimuli = _.shuffle(makeSecondStims())

var third_stimuli = _.shuffle(makeSecondStims())

function makeStims() {
	stims = [];

	predsUsed = [];

	for (var i=0; i<nouns.length; i+=2) { // 8 nouns * 2 = 12 (3 x 2 x 8) --> 8 categories x 2 random positive, negative, neutral predicates 
		noun1 = nouns[i];
		noun2 = nouns[i+1];

		var positive_count = 0;
		var negative_count = 0;

		if (predsUsed.length == predicates.length){
			predsUsed.length = 0;
			predicates = _.shuffle(predicates);
		} 

		for (var j = 0; j < predicates.length; j++)
		{

			pred = predicates[j];

			if (pred.Class == 'positive' && positive_count < 2 &&predsUsed.indexOf(pred) == -1)
			{
				stims.push(
				{
					"Predicate":pred.Predicate,
					"Class":pred.Class,				
					"Noun":noun1.Noun,
					"NounClass":noun1.NounClass,
					"POS":pred.POS
				}
				);
				stims.push(
				{
					"Predicate":pred.Predicate,
					"Class":pred.Class,				
					"Noun":noun2.Noun,
					"NounClass":noun2.NounClass,
					"POS":pred.POS
				}
				);
				positive_count += 1;
				predsUsed.push(pred);
			}
			else 
			{
				if (pred.Class == 'negative' && negative_count < 2&&predsUsed.indexOf(pred) == -1)
				{
				stims.push(
				{
					"Predicate":pred.Predicate,
					"Class":pred.Class,				
					"Noun":noun1.Noun,
					"NounClass":noun1.NounClass,
					"POS":pred.POS
				}
				);
				stims.push(
				{
					"Predicate":pred.Predicate,
					"Class":pred.Class,				
					"Noun":noun2.Noun,
					"NounClass":noun2.NounClass,
					"POS":pred.POS
				}
				);
				negative_count += 1;
				predsUsed.push(pred);
				}

			}

		}

		
	}

	for (var k = 0; k < nouns.length; k+=2)
	{
		noun1 = nouns[k];
		noun2 = nouns[k+1];

		neutral = neutrals[k - (k/2.0)];

		stims.push(
				{
					"Predicate":neutral.Predicate,
					"Class":neutral.Class,				
					"Noun":noun1.Noun,
					"NounClass":noun1.NounClass,
					"POS":neutral.POS
				}
				);
		stims.push(
				{
					"Predicate":neutral.Predicate,
					"Class":neutral.Class,				
					"Noun":noun2.Noun,
					"NounClass":noun2.NounClass,
					"POS":neutral.POS
				}
				);



	}

	stims.push(
	{
		"Predicate":"matter",
		"Class": "positive",
		"Noun":"Black lives",
		"NounClass":"black_lives",
		"POS":"verb"
	});

	stims.push(
	{
		"Predicate":"lay eggs",
		"Class": "true_filler",
		"Noun":"chickens",
		"NounClass":"true_filler",
		"POS":"verb"
	});

	stims.push(
	{
		"Predicate":"eat carrots",
		"Class": "true_filler",
		"Noun":"rabbits",
		"NounClass":"true_filler",
		"POS":"verb"
	});

	stims.push(
	{
		"Predicate":"fly",
		"Class": "false_filler",
		"Noun":"pigs",
		"NounClass":"false_filler",
		"POS":"verb"
	});

	stims.push(
	{
		"Predicate":"talk",
		"Class": "false_filler",
		"Noun":"cats",
		"NounClass":"false_filler",
		"POS":"verb"
	});
		
	return stims;
	
}

//function for part 2 
function makeSecondStims(){
	stims = []

	for (var i = 0; i < nouns.length; i++){
		noun = nouns[i];
		stims.push(
		{
			"Noun":noun.Noun,
			"NounClass":noun.NounClass,
			"Singular":noun.Singular
		}
		);
	}

	stims.push(
		{
			"Noun":"Black people",
			"NounClass":"black",
			"Singular":"Black person"
		}
		);

	return stims;
}
