using AudioSchedules: @q_str, justly
using FileIO: save
import LibSndFile
using Unitful: Hz, s

cd("/home/brandon/Music")

MAJOR_6 = (
    (q"1" => 1, q"1" => 6),
    (q"1" => 1, q"3/2" => 5),
    (q"1" => 1, q"2" => 4),
    (q"1" => 1, q"5/4o1" => 3),
    (q"1" => 1, q"2" => 2),
    (q"1" => 1, q"3/2" => 1),
)
MINOR_6 = (
    (q"1" => 1, q"1" => 6),
    (q"1" => 1, q"3/2" => 5),
    (q"1" => 1, q"2" => 4),
    (q"1" => 1, q"6/5o1" => 3),
    (q"1" => 1, q"2" => 2),
    (q"1" => 1, q"3/2" => 1),
)
MAJOR_3 = (
    (q"1" => 1, q"1" => 3),
    (q"1" => 1, q"3/2" => 2),
    (q"1" => 1, q"5/4o1" => 1),
)
MINOR_3 = (
    (q"1" => 1, q"1" => 3),
    (q"1" => 1, q"3/2" => 2),
    (q"1" => 1, q"6/5o1" => 1),
)
SONG = (
    (q"1" => 0,), # Alas my love you
    MINOR_6...,
    (q"9/5o-1" => 0,), # do me wrong to
    MAJOR_6...,
    (q"5/9o1" => 0,), # cast me off so
    MINOR_6...,
    (q"3/2o-1" => 0,), # courteously; And
    MAJOR_6...,
    (q"4/5o1" => 0,), # I have loved you
    MAJOR_6...,
    (q"3/2o-1" => 0,), # oh so long, de-
    MAJOR_6...,
    (q"5/9o1" => 0,), # lighting
    MINOR_3...,
    (q"3/2o-1" => 0,), # in your
    MAJOR_3...,
    (q"2/3o1" => 0,), # company.
    MINOR_6...,
    (q"6/5" => 0,), # Greensleeves was
    MAJOR_6...,
    (q"3/2o-1" => 0,), # my delight,
    MAJOR_6...,
    (q"5/9o1" => 0,), # Greensleeves my
    MINOR_6...,
    (q"3/2o-1" => 0,), # heart of gold
    MAJOR_6...,
    (q"4/5o1" => 0,), # Greensleeves my
    MAJOR_6...,
    (q"3/2o-1" => 0,), # heart of joy
    MAJOR_6...,
    (q"5/9o1" => 0,), # who but my
    MINOR_3...,
    (q"3/2o-1" => 0,), # lady
    MAJOR_3...,
    (q"2/3o1" => 0,), # Greensleeves
    MINOR_6...
)

a_schedule = justly(SONG, 44100Hz, key = 176Hz, seconds_per_beat = 0.2s)
save("greensleeves.wav", read(a_schedule, length(a_schedule)))
