# teaching words
nibo = 'nibo'
fale = 'fale'
bofa = 'bofa'
leni = 'leni'
lebo = 'lebo'
fani = 'fani'
boni = 'boni'
lefa = 'lefa'
fabo = 'fabo'
nile = 'nile'
bole = 'bole'
nifa = 'nifa'

# CD2 generalization words

nibe = 'nibe'
lofi = 'lofi'
bofi = 'bofi'
nale = 'nale'
leba = 'leba'
nofa = 'nofa'
bona = 'bona'
lefi = 'lefi'
fabe = 'fabe'
nilo = 'nilo'
febi = 'febi'
lano = 'lano'

# CD1 generalization words
lani = 'lani'
febo = 'febo'
nole = 'nole'
bifa = 'bifa'

# constant generalization words
falo = 'falo'
bena = 'bena'

constant = [
    falo,
    bena
]

per_cycle = {
    1 : {'teaching': [nibo, fale], 'generalization': [nibe, lofi]},
    2 : {'teaching': [bofa, leni], 'generalization': [bofi, nale]},
    3 : {'teaching': [lebo, fani], 'generalization': [leba, nofa]},
    4 : {'teaching': [boni, lefa], 'generalization': [bona, lefi]},
    5 : {'teaching': [fabo, nile], 'generalization': [fabe, nilo]},
    6 : {'teaching': [bole, nifa], 'generalization': [febi, lano]},
}

pre_test_hardcorded_order = [
    nibo,
    fale,
    bofa,
    leni,
    lebo,
    fani,
    boni,
    lefa,
    fabo,
    nile,
    bole,
    nifa,

    bona,
    lefi,
    fabe,
    nilo,

    lani,
    febo,
    nole,
    bifa
    ]