// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]


//For male/female speaker: make default story, replace 

//Implement priors - 8 slides "How X do you think Y are?"

var allStimuli = [

		//competent
		//2 qud, 2 categories, 1 predicate, speakers (male/female)

		//Dems competent, who
		//Dems competent, are
		//Reps competent, who
		//Reps competent, are

		[{"CoverStory":"Michelle is a senator of a major political party and is a member of many legislative committees. Every legislative committee is composed of either all Democrats or all Republicans. The other day in a Senate session, the head of the Senate unveiled that he would be choosing an existing legislative committee to work on a new project based on their competence. Michelle raised her hand to speak in the meeting and said,","Statement":"Democrats are competent.","CritNoun":"Democrats","OtherNoun":"Republicans","Predicate":"competent","Opposite":"incompetent","NounClass":"political_party","QUD":"who","Comprehension":"head of the Senate/legislative committee","SpeakerOrder":"female/male"},
		{"CoverStory":"Michelle is a senator of a major political party and is a member of many legislative committees. Every legislative committee is composed of either all Democrats or all Republicans. The other day in a Senate session, the head of the Senate unveiled that he would be choosing two existing legislative committees to work on a new, highly classified project based on their competence, and that he had already chosen one team of Democrats. She raised her hand to speak in the meeting and said,","Statement":"Democrats are competent.","CritNoun":"Democrats","OtherNoun":"Republicans","Predicate":"competent","Opposite":"incompetent","NounClass":"political_party","QUD":"are","Comprehension":"head of the Senate/legislative committee","SpeakerOrder":"female/male"},
		{"CoverStory":"Michelle is a senator of a major political party and is a member of many legislative committees. Every legislative committee is composed of either all Democrats or all Republicans. The other day in a Senate session, the head of the Senate unveiled that he would be choosing an existing legislative committee to work on a new project based on their competence. Michelle raised her hand to speak in the meeting and said,","Statement":"Republicans are competent.","CritNoun":"Republicans","OtherNoun":"Democrats","Predicate":"competent","Opposite":"incompetent","NounClass":"political_party","QUD":"who","Comprehension":"head of the Senate/legislative committee","SpeakerOrder":"female/male"},
		{"CoverStory":"Michelle is a senator of a major political party and is a member of many legislative committees. Every legislative committee is composed of either all Democrats or all Republicans. The other day in a Senate session, the head of the Senate unveiled that he would be choosing two existing legislative committees to work on a new, highly classified project based on their competence, and that he had already chosen one team of Republicans. She raised her hand to speak in the meeting and said,","Statement":"Republicans are competent.","CritNoun":"Republicans","OtherNoun":"Democrats","Predicate":"competent","Opposite":"incompetent","NounClass":"political_party","QUD":"are","Comprehension":"head of the Senate/legislative committee","SpeakerOrder":"female/male"},
		{"CoverStory":"David is a senator of a major political party and is a member of many legislative committees. Every legislative committee is composed of either all Democrats or all Republicans. The other day in a Senate session, the head of the Senate unveiled that she would be choosing an existing legislative committee to work on a new project based on their competence. David raised his hand to speak in the meeting and said,","Statement":"Democrats are competent.","CritNoun":"Democrats","OtherNoun":"Republicans","Predicate":"competent","Opposite":"incompetent","NounClass":"political_party","QUD":"who","Comprehension":"head of the Senate/legislative committee","SpeakerOrder":"male/female"},
		{"CoverStory":"David is a senator of a major political party and is a member of many legislative committees. Every legislative committee is composed of either all Democrats or all Republicans. The other day in a Senate session, the head of the Senate unveiled that she would be choosing two existing legislative committees to work on a new, highly classified project based on their competence, and that she had already chosen one team of Republicans. David raised his hand to speak in the meeting and said,","Statement":"Democrats are competent.","CritNoun":"Democrats","OtherNoun":"Republicans","Predicate":"competent","Opposite":"incompetent","NounClass":"political_party","QUD":"are","Comprehension":"head of the Senate/legislative committee","SpeakerOrder":"male/female"},
		{"CoverStory":"David is a senator of a major political party and is a member of many legislative committees. Every legislative committee is composed of either all Democrats or all Republicans. The other day in a Senate session, the head of the Senate unveiled that she would be choosing an existing legislative committee to work on a new project based on their competence. David raised his hand to speak in the meeting and said,","Statement":"Republicans are competent.","CritNoun":"Republicans","OtherNoun":"Democrats","Predicate":"competent","Opposite":"incompetent","NounClass":"political_party","QUD":"who","Comprehension":"head of the Senate/legislative committee","SpeakerOrder":"male/female"},
		{"CoverStory":"David is a senator of a major political party and is a member of many legislative committees. Every legislative committee is composed of either all Democrats or all Republicans. The other day in a Senate session, the head of the Senate unveiled that she would be choosing two existing legislative committees to work on a new, highly classified project based on their competence, and that he had already chosen one team of Democrats. She raised her hand to speak in the meeting and said,","Statement":"Republicans are competent.","CritNoun":"Republicans","OtherNoun":"Democrats","Predicate":"competent","Opposite":"incompetent","NounClass":"political_party","QUD":"are","Comprehension":"head of the Senate/legislative committee","SpeakerOrder":"male/female"}
		],

		//rational
		//who men
		[{"CoverStory":"Michelle is a lawyer at a major law firm and is a member of many litigation teams. Every litigation team at her firm is composed of either all men or all women. The other day in an all-hands meeting, one of the partners unveiled that he would be choosing an existing litigation team to work on a new high-profile case based on their rationality. Michelle raised her hand to speak in the meeting and said,","Statement":"Men are rational.","CritNoun":"men","OtherNoun":"women","Predicate":"rational","Opposite":"emotional","NounClass":"gender","QUD":"who","Comprehension":"partner/litigation team","SpeakerOrder":"female/male"},
		
		//are men
		{"CoverStory":"Michelle is a lawyer at a major law firm and is a member of many litigation teams. Every litigation team at her firm is composed of either all men or all women. The other day in an all-hands meeting, one of the partners unveiled that he would be choosing two existing litigation teams to work on a new high-profile case based on their rationality, and that he had already chosen one team of women. Michelle raised her hand to speak in the meeting and said,","Statement":"Men are rational.","CritNoun":"men","OtherNoun":"women","Predicate":"rational","Opposite":"emotional","NounClass":"gender","QUD":"are","Comprehension":"partner/litigation team","SpeakerOrder":"female/male"},
		
		//who women
		{"CoverStory":"Michelle is a lawyer at a major law firm and is a member of many litigation teams. Every litigation team at her firm is composed of either all men or all women. The other day in an all-hands meeting, one of the partners unveiled that he would be choosing an existing litigation team to work on a new high-profile case based on their rationality. Michelle raised her hand to speak in the meeting and said,","Statement":"Women are rational.","CritNoun":"women","OtherNoun":"men","Predicate":"rational","Opposite":"emotional","NounClass":"gender","QUD":"who","Comprehension":"partner/litigation team","SpeakerOrder":"female/male"},
		
		//are women 
		{"CoverStory":"Michelle is a lawyer at a major law firm and is a member of many litigation teams. Every litigation team at her firm is composed of either all men or all women. The other day in an all-hands meeting, one of the partners unveiled that he would be choosing two existing litigation teams to work on a new high-profile case based on their rationality, and that he had already chosen one team of men. Michelle raised her hand to speak in the meeting and said,","Statement":"Women are rational.","CritNoun":"women","OtherNoun":"men","Predicate":"rational","Opposite":"emotional","NounClass":"gender","QUD":"are","Comprehension":"partner/litigation team","SpeakerOrder":"female/male"},
		
		//who men
		{"CoverStory":"David is a lawyer at a major law firm and is a member of many litigation teams. Every litigation team at his firm is composed of either all men or all women. The other day in an all-hands meeting, one of the partners unveiled that she would be choosing an existing litigation team to work on a new high-profile case based on their rationality. David raised his hand to speak in the meeting and said,","Statement":"Men are rational.","CritNoun":"men","OtherNoun":"women","Predicate":"rational","Opposite":"emotional","NounClass":"gender","QUD":"who","Comprehension":"partner/litigation team","SpeakerOrder":"male/female"},
		
//are men
		{"CoverStory":"David is a lawyer at a major law firm and is a member of many litigation teams. Every litigation team at his firm is composed of either all men or all women. The other day in an all-hands meeting, one of the partners unveiled that she would be choosing two existing litigation teams to work on a new high-profile case based on their rationality, and that he had already chosen one team of women. David raised his hand to speak in the meeting and said,","Statement":"Men are rational.","CritNoun":"men","OtherNoun":"women","Predicate":"rational","Opposite":"emotional","NounClass":"gender","QUD":"are","Comprehension":"partner/litigation team","SpeakerOrder":"male/female"},
		
//who women
		{"CoverStory":"David is a lawyer at a major law firm and is a member of many litigation teams. Every litigation team at his firm is composed of either all men or all women. The other day in an all-hands meeting, one of the partners unveiled that she would be choosing an existing litigation team to work on a new high-profile case based on their rationality. David raised his hand to speak in the meeting and said,","Statement":"Women are rational.","CritNoun":"women","OtherNoun":"men","Predicate":"rational","Opposite":"emotional","NounClass":"gender","QUD":"who","Comprehension":"partner/litigation team","SpeakerOrder":"male/female"},
		
//are women
		{"CoverStory":"David is a lawyer at a major law firm and is a member of many litigation teams. Every litigation team at his firm is composed of either all men or all women. The other day in an all-hands meeting, one of the partners unveiled that she would be choosing two existing litigation teams to work on a new high-profile case based on their rationality, and that he had already chosen one team of men. David raised his hand to speak in the meeting and said,","Statement":"Women are rational.","CritNoun":"women","OtherNoun":"men","Predicate":"rational","Opposite":"emotional","NounClass":"gender","QUD":"are","Comprehension":"partner/litigation team","SpeakerOrder":"male/female"}
		],

		//polite
		[{"CoverStory":"Michelle is a waitress at a five-star restaurant and is a member of many hospitality teams within the larger waitstaff. Every hospitality team at the restaurant is composed of either all people in their 20's or all people in their 50's. The other day in an all-hands meeting before opening for the day, the owner of the restaurant unveiled that he would be choosing an existing hospitality team to work a high-profile, celebrity charity dinner event based on their politeness. Michelle raised her hand to speak in the meeting and said,","Statement":"People in their 20's are polite.","CritNoun":"People in their 20's","OtherNoun":"People in their 50's","Predicate":"polite","Opposite":"rude","NounClass":"age","QUD":"who","Comprehension":"owner/hospitality team","SpeakerOrder":"female/male"},
		{"CoverStory":"Michelle is a waitress at a five-star restaurant is a member of many hospitality teams within the larger waitstaff. Every hospitality team at the restaurant is composed of either all people in their 20's or all people in their 50's. The other day in an all-hands meeting before opening for the day, the owner of the restaurant unveiled that he would be choosing two existing hospitality teams to work a high-profile, celebrity charity dinner event based on their politeness, and that he had already chosen one team of people in their 50's. Michelle raised her hand to speak in the meeting and said,","Statement":"People in their 20's are polite.","CritNoun":"People in their 20's","OtherNoun":"People in their 50's","Predicate":"polite","Opposite":"rude","NounClass":"age","QUD":"are","Comprehension":"owner/hospitality team","SpeakerOrder":"female/male"},
		{"CoverStory":"Michelle is a waitress at a five-star restaurant and is a member of many hospitality teams within the larger waitstaff. Every hospitality team at the restaurant is composed of either all people in their 20's or all people in their 50's. The other day in an all-hands meeting before opening for the day, the owner of the restaurant unveiled that he would be choosing an existing hospitality team to work a high-profile, celebrity charity dinner event based on their politeness. Michelle raised her hand to speak in the meeting and said,","Statement":"People in their 50's are polite.","CritNoun":"People in their 50's","OtherNoun":"People in their 20's","Predicate":"polite","Opposite":"rude","NounClass":"age","QUD":"who","Comprehension":"owner/hospitality team","SpeakerOrder":"female/male"},
		{"CoverStory":"Michelle is a waitress at a five-star restaurant is a member of many hospitality teams within the larger waitstaff. Every hospitality team at the restaurant is composed of either all people in their 20's or all people in their 50's. The other day in an all-hands meeting before opening for the day, the owner of the restaurant unveiled that he would be choosing two existing hospitality teams to work a high-profile, celebrity charity dinner event based on their politeness, and that he had already chosen one team of people in their 20's. Michelle raised her hand to speak in the meeting and said,","Statement":"People in their 50's are polite.","CritNoun":"People in their 50's","OtherNoun":"People in their 20's","Predicate":"polite","Opposite":"rude","NounClass":"age","QUD":"are","Comprehension":"owner/hospitality team","SpeakerOrder":"female/male"},
		{"CoverStory":"David is a waiter at a five-star restaurant and is a member of many hospitality teams within the larger waitstaff. Every hospitality team at the restaurant is composed of either all people in their 20's or all people in their 50's. The other day in an all-hands meeting before opening for the day, the owner of the restaurant unveiled that she would be choosing an existing hospitality team to work a high-profile, celebrity charity dinner event based on their politeness. David raised his hand to speak in the meeting and said,","Statement":"People in their 20's are polite.","CritNoun":"People in their 20's","OtherNoun":"People in their 50's","Predicate":"polite","Opposite":"rude","NounClass":"age","QUD":"who","Comprehension":"owner/hospitality team","SpeakerOrder":"male/female"},
		{"CoverStory":"David is a waiter at a five-star restaurant is a member of many hospitality teams within the larger waitstaff. Every hospitality team at the restaurant is composed of either all people in their 20's or all people in their 50's. The other day in an all-hands meeting before opening for the day, the owner of the restaurant unveiled that she would be choosing two existing hospitality teams to work a high-profile, celebrity charity dinner event based on their politeness, and that he had already chosen one team of people in their 50's. David raised his hand to speak in the meeting and said,","Statement":"People in their 20's are polite.","CritNoun":"People in their 20's","OtherNoun":"People in their 50's","Predicate":"polite","Opposite":"rude","NounClass":"age","QUD":"are","Comprehension":"owner/hospitality team","SpeakerOrder":"male/female"},
		{"CoverStory":"David is a waiter at a five-star restaurant and is a member of many hospitality teams within the larger waitstaff. Every hospitality team at the restaurant is composed of either all people in their 20's or all people in their 50's. The other day in an all-hands meeting before opening for the day, the owner of the restaurant unveiled that she would be choosing an existing hospitality team to work a high-profile, celebrity charity dinner event based on their politeness. David raised his hand to speak in the meeting and said,","Statement":"People in their 50's are polite.","CritNoun":"People in their 50's","OtherNoun":"People in their 20's","Predicate":"polite","Opposite":"rude","NounClass":"age","QUD":"who","Comprehension":"owner/hospitality team","SpeakerOrder":"male/female"},
		{"CoverStory":"David is a waiter at a five-star restaurant is a member of many hospitality teams within the larger waitstaff. Every hospitality team at the restaurant is composed of either all people in their 20's or all people in their 50's. The other day in an all-hands meeting before opening for the day, the owner of the restaurant unveiled that she would be choosing two existing hospitality teams to work a high-profile, celebrity charity dinner event based on their politeness, and that he had already chosen one team of people in their 20's. David raised his hand to speak in the meeting and said,","Statement":"People in their 50's are polite.","CritNoun":"People in their 50's","OtherNoun":"People in their 20's","Predicate":"polite","Opposite":"rude","NounClass":"age","QUD":"are","Comprehension":"owner/hospitality team","SpeakerOrder":"male/female"}
		],

		//smart

		//order is who are who are (x 2 for female and male speakers)

		[{"CoverStory":"Michelle is a student at an international high school and is a member of many study groups. Every study group in her school is composed of either all American or all European students. The other day in an all-school assembly, the principal unveiled that he would be choosing an existing study group to receive a prestigious scholarship based on their intelligence. Michelle raised her hand to speak in the assembly and said,","Statement":"Americans are smart.","CritNoun":"Americans","OtherNoun":"Europeans","Predicate":"smart","Opposite":"stupid","NounClass":"nationality","QUD":"who","Comprehension":"principal/study group","SpeakerOrder":"female/male"},
		{"CoverStory":"Michelle is a student at an international high school and is a member of many study groups. Every study group in her school is composed of either all American or all European students. The other day in an all-school assembly, the principal unveiled that he would be choosing two existing study groups to receive a prestigious scholarship based on their intelligence, and that he'd already chosen one group of Europeans. Michelle raised her hand to speak in the assembly and said,","Statement":"Americans are smart.","CritNoun":"Americans","OtherNoun":"Europeans","Predicate":"smart","Opposite":"stupid","NounClass":"nationality","QUD":"are","Comprehension":"principal/study group","SpeakerOrder":"female/male"},
		{"CoverStory":"Michelle is a student at an international high school and is a member of many study groups. Every study group in her school is composed of either all American or all European students. The other day in an all-school assembly, the principal unveiled that he would be choosing an existing study group to receive a prestigious scholarship based on their intelligence. Michelle raised her hand to speak in the assembly and said,","Statement":"Europeans are smart.","CritNoun":"Europeans","OtherNoun":"Americans","Predicate":"smart","Opposite":"stupid","NounClass":"nationality","QUD":"who","Comprehension":"principal/study group","SpeakerOrder":"female/male"},
		{"CoverStory":"Michelle is a student at an international high school and is a member of many study groups. Every study group in her school is composed of either all American or all European students. The other day in an all-school assembly, the principal unveiled that he would be choosing two existing study groups to receive a prestigious scholarship based on their intelligence, and that he'd already chosen one group of Americans. Michelle raised her hand to speak in the assembly and said,","Statement":"Europeans are smart.","CritNoun":"Europeans","OtherNoun":"Americans","Predicate":"smart","Opposite":"stupid","NounClass":"nationality","QUD":"are","Comprehension":"principal/study group","SpeakerOrder":"female/male"},
		//who americans
		{"CoverStory":"David is a student at an international high school and is a member of many study groups. Every study group in his school is composed of either all American or all European students. The other day in an all-school assembly, the principal unveiled that she would be choosing an existing study group to receive a prestigious scholarship based on their intelligence. David raised his hand to speak in the assembly and said,","Statement":"Americans are smart.","CritNoun":"Americans","OtherNoun":"Europeans","Predicate":"smart","Opposite":"stupid","NounClass":"nationality","QUD":"who","Comprehension":"principal/study group","SpeakerOrder":"male/female"},
		//are americans
		{"CoverStory":"David is a student at an international high school and is a member of many study groups. Every study group in his school is composed of either all American or all European students. The other day in an all-school assembly, the principal unveiled that she would be choosing two existing study groups to receive a prestigious scholarship based on their intelligence, and that she'd already chosen one group of Europeans. David raised his hand to speak in the assembly and said,","Statement":"Americans are smart.","CritNoun":"Americans","OtherNoun":"Europeans","Predicate":"smart","Opposite":"stupid","NounClass":"nationality","QUD":"are","Comprehension":"principal/study group","SpeakerOrder":"male/female"},
		//who europeans
		{"CoverStory":"David is a student at an international high school and is a member of many study groups. Every study group in his school is composed of either all American or all European students. The other day in an all-school assembly, the principal unveiled that she would be choosing an existing study group to receive a prestigious scholarship based on their intelligence. David raised his hand to speak in the assembly and said,","Statement":"Europeans are smart.","CritNoun":"Europeans","OtherNoun":"Americans","Predicate":"smart","Opposite":"stupid","NounClass":"nationality","QUD":"who","Comprehension":"principal/study group","SpeakerOrder":"male/female"},
		//are europeans
		{"CoverStory":"David is a student at an international high school and is a member of many study groups. Every study group in his school is composed of either all American or all European students. The other day in an all-school assembly, the principal unveiled that she would be choosing two existing study groups to receive a prestigious scholarship based on their intelligence, and that she'd already chosen one group of Americans. David raised his hand to speak in the assembly and said,","Statement":"Europeans are smart.","CritNoun":"Europeans","OtherNoun":"Americans","Predicate":"smart","Opposite":"stupid","NounClass":"nationality","QUD":"are","Comprehension":"principal/study group","SpeakerOrder":"male/female"}
		]

]

var priors = [
	{"Noun":"Democrats","Predicate":"competent","Opposite":"incompetent"},
	{"Noun":"Republicans","Predicate":"competent","Opposite":"incompetent"},
	{"Noun":"men","Predicate":"rational","Opposite":"emotional"},
	{"Noun":"women","Predicate":"rational","Opposite":"emotional"},
	{"Noun":"Europeans","Predicate":"smart","Opposite":"stupid"},
	{"Noun":"people in their 20's","Predicate":"polite","Opposite":"rude"},
	{"Noun":"people in their 50's","Predicate":"polite","Opposite":"rude"},
	{"Noun":"Americans","Predicate":"smart","Opposite":"stupid"}
]

var stimuli = _.shuffle(makeStims());

var secondStimuli = _.shuffle(priors);

//must be same number of who's and are's 
function makeStims() {
	stims = [];

	are = 0;
	who = 0;

	for (var i = 0; i < allStimuli.length; i++){
		choices = allStimuli[i];
		sampledStim = _.sample(choices);

		if (sampledStim.QUD == 'who'){

			if (who < 2){
				stims.push(sampledStim);
				who++;
			}
			else
			{
				while (sampledStim.QUD!='are')
				{
					sampledStim = _.sample(choices);
				}
				stims.push(sampledStim);
				are++;
			}
		}

		else if (sampledStim.QUD == 'are'){

			if (are < 2){
				stims.push(sampledStim);
				are++;
			}
			else
			{
				while (sampledStim.QUD =='are')
				{
					sampledStim = _.sample(choices);
				}
				stims.push(sampledStim);
				who++;
			}
		}
	}

	return stims;
	
}
