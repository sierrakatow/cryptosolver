import nltk
import itertools
import operator

solutionslist = open ("answer.txt").readlines()
#reads the text file of possible solutions into a list of solutions

solutions_tagged = [nltk.pos_tag(nltk.word_tokenize(solution)) for solution in solutionslist] 
# word_tokenize(solution) takes each solution (for example, "The cat jumped.") and changes it into a list of "tokens," as follows: ['The', 'cat', 'jumped', '.'] pos_tag then tags each token with its correct part of speech and returns a list of tuples: [('The', 'DT'), ('cat', 'NN'), ('jumped', 'VBD'), ('.', '.')]

output = [zip(*solution) for solution in solutions_tagged]

grammar = nltk.parse_cfg("""
	S -> NP VP
	NP -> 'DT' N | N
	VP -> V | V N
	V -> 'VB' | 'VBD'
	N -> 'NN' | 'NNS' | 'NNP'
""")

parser = nltk.ChartParser(grammar)

parse_check = [parser.nbest_parse(pos_list) for (words_list, pos_list) in output]

good_indices = [i for i,x in enumerate(parse_check) if x != []]

good_sentences = [words_list for i,(words_list,pos_list) in enumerate(output) if i in good_indices]

bad_sentences = [words_list for i,(words_list,pos_list) in enumerate(output) if i not in good_indices]

for split_list in good_sentences:
	print ' '.join(split_list)

for split_list in bad_sentences:
	print ' '.join(split_list)