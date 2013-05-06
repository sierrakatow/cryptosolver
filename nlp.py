import nltk
import itertools
import operator

solutionslist = open ("answer.txt").readlines()
#reads the text file of possible solutions into a list of solutions

solutions_tagged = [nltk.pos_tag(nltk.word_tokenize(solution)) for solution in solutionslist] 
# word_tokenize(solution) takes each solution (for example, "The cat jumped.") and changes it into a list of "tokens," as follows: ['The', 'cat', 'jumped', '.'] pos_tag then tags each token with its correct part of speech and returns a list of tuples: [('The', 'DT'), ('cat', 'NN'), ('jumped', 'VBD'), ('.', '.')]

output = [zip(*solution) for solution in solutions_tagged]

grammar = nltk.parse_cfg("""
	S -> NP VP | VP | S C S
	NP -> D N1 | N1 | NP PP | PN | NP 'CC' NP | GP
	VP -> V | V NP | VP PP | VP ADVP | 'MD' VP
        PP -> P NP 
        ADVP -> ADV | ADV ADVP | ADVP 'CC' ADVP
        AP -> A | ADVP A | AP 'CC' AP
        GP -> G | G N | GP PP | GP ADVP 
        N1 -> N | AP N
	V -> 'VB' | 'VBD' | 'VBP' | 'VBZ'
	N -> 'NN' | 'NNS' | 'EX'
        PN -> 'PRP' | 'NNP' | 'NNPS' | 'WP'
        A -> 'JJ' | 'JJR' | 'JJS' | 'CD'
        ADV -> 'RB'| 'RBR'| 'RBS'
        P -> 'IN' | 'TO'
        D -> 'DT' | 'PDT' | 'WDT' | 'PRP$' | 'WP$' 
        C -> 'CC' | 'WRB'
        G -> 'VBG'
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
