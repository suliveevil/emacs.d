#!/usr/bin/env python3

# https://gist.github.com/vivekhaldar/63764b085640bd82ebf07482f8a58cb8

import whisper
import sys
import string
import math
from stable_whisper import modify_model
from stable_whisper import stabilize_timestamps
from stable_whisper import results_to_word_srt
from stable_whisper import results_to_word_srt
from moviepy.editor import AudioClip, VideoFileClip, concatenate_videoclips
MODEL = 'base'
# Store phonetic alphabet in a dictionary.
phonetic_alphabet = {
  'A': 'Alpha',
  'B': 'Bravo',
  'C': 'Charlie',
  'D': 'Delta',
  'E': 'Echo',
  'F': 'Foxtrot',
  'G': 'Golf',
  'H': 'Hotel',
  'I': 'India',
  'J': 'Juliett',
  'K': 'Kilo',
  'L': 'Lima',
  'M': 'Mike',
  'N': 'November',
  'O': 'Oscar',
  'P': 'Papa',
  'Q': 'Quebec',
  'R': 'Romeo',
  'S': 'Sierra',
  'T': 'Tango',
  'U': 'Uniform',
  'V': 'Victor',
  'W': 'Whiskey',
  'X': 'X',
  'Y': 'Yankee',
  'Z': 'Zulu',
}

# Commands
class Commands:
  COMMAND_PREFIX = 'victor'
  keep_segment = [COMMAND_PREFIX, 'kilo']
  drop_segment = [COMMAND_PREFIX, 'delta']
  chapter_break = [COMMAND_PREFIX, 'charlie']
  COMMANDS = {
    'keep_segment': keep_segment,
    'drop_segment': drop_segment,
    'chapter_break': chapter_break,
  }
input_file = sys.argv[1]
output_file = sys.argv[2]
def speech_to_text():
  model = whisper.load_model(MODEL)
  modify_model(model)
  print('Speech to text...')
  results = model.transcribe(input_file)
  print('... done!\n')
  return results
def word_level_timestamps(parsed_speech_to_text):
  print('Getting word-level timestamps...')
  word_segments = stabilize_timestamps(parsed_speech_to_text, top_focus=True)
  #print(word_segments)
  print('... done!\n')
  return word_segments
def string_canonical(s):
  # Remove punctuation.
  no_punc = s.translate(str.maketrans('', '', string.punctuation))
  return no_punc.strip().lower()

# Return timestamp of (i + 1)th word. Used to get the ending timestamp
# of a command.
def timestamp_of_next_word(seg, i):
  word_timestamps = seg['whole_word_timestamps']
  num_words = len(word_timestamps)
  if (i + 1) >= num_words:
    # No more words in this segment.
    return seg['end']
  else:
    return seg['whole_word_timestamps'][i + 1]['timestamp']

def find_command_in_segment(seg):
  word_timestamps = seg['whole_word_timestamps']
  num_words = len(word_timestamps)
  i = 0
  while i < num_words:
    word = string_canonical(word_timestamps[i]['word'])
    # print(f'{word} ')
    if word == Commands.COMMAND_PREFIX:
      #print('! ')
      time_command_start = word_timestamps[i]['timestamp']
      i += 1
      if i > num_words:
        break
      second_word_of_command = string_canonical(word_timestamps[i]['word'])
      match second_word_of_command:
        case 'kilo':
          time_command_end = timestamp_of_next_word(seg, i)
          return Commands.keep_segment, time_command_start, time_command_end
        case 'delta':
          time_command_end = timestamp_of_next_word(seg, i)
          return Commands.drop_segment, time_command_start, time_command_end
        case _:
          continue
    i += 1

def find_commands(timestamps):
  commands = []
  for segment in timestamps:
    seg_command = find_command_in_segment(segment)
    if seg_command:
      commands.append(seg_command)
  return commands

# Hack. When dropping a segment I see a split-second of the end of
# the dropped segment. So add a small fuzz factor for that.
DROP_SEGMENT_DELTA = 0.0
KEEP_SEGMENT_DELTA = 0.8

# Returns list of (start, end) tuples of intervals to keep.
def intervals_to_keep(commands):
  keep_intervals = []
  keep_start, keep_end = 0, 0
  for command_timestamp in commands:
    cmd, begin_ts, end_ts = command_timestamp
    match cmd:
      case Commands.keep_segment:
        # Keep until the start of the command.
        keep_end = begin_ts - KEEP_SEGMENT_DELTA
        keep_intervals.append([keep_start, keep_end])
        # Next (possibly) starts at end of command.
        keep_start = end_ts
      case Commands.drop_segment:
        # Next (possibly) starts at end of command.
        keep_start = end_ts + DROP_SEGMENT_DELTA
      case _:
        print(f'Eeek! Unrecognized command: {cmd}')
  return keep_intervals
# Iterate over audio to find the non-silent parts. Outputs a list of
# (speaking_start, speaking_end) intervals.
# Args:
#  window_size: (in seconds) hunt for silence in windows of this size
#  volume_threshold: volume below this threshold is considered to be silence
#  ease_in: (in seconds) add this much silence around speaking intervals
def find_speaking_intervals(audio_clip, window_size=0.1, volume_threshold=0.05, ease_in=0.1, audio_fps=44100):
  # First, iterate over audio to find all silent windows.
  num_windows = math.floor(audio_clip.end/window_size)
  window_is_silent = []
  for i in range(num_windows):
    s = audio_clip.subclip(i * window_size, (i + 1) * window_size).set_fps(audio_fps)
    v = s.max_volume()
    window_is_silent.append(v < volume_threshold)

  # Find speaking intervals.
  speaking_start = 0
  speaking_end = 0
  speaking_intervals = []
  for i in range(1, len(window_is_silent)):
    e1 = window_is_silent[i - 1]
    e2 = window_is_silent[i]
    # silence -> speaking
    if e1 and not e2:
      speaking_start = i * window_size
    # speaking -> silence, now have a speaking interval
    if not e1 and e2:
      speaking_end = i * window_size
      new_speaking_interval = [max(0, speaking_start - ease_in), speaking_end + ease_in]
      # With tiny windows, this can sometimes overlap the previous window, so merge.
      need_to_merge = len(speaking_intervals) > 0 and speaking_intervals[-1][1] > new_speaking_interval[0]
      if need_to_merge:
        merged_interval = [speaking_intervals[-1][0], new_speaking_interval[1]]
        speaking_intervals[-1] = merged_interval
      else:
        speaking_intervals.append(new_speaking_interval)
  return speaking_intervals

def find_speaking(input_clip, input_audio_fps):
  print("\n\n\n----- Now cutting out dead air... -----")
  speaking_intervals = find_speaking_intervals(input_clip.audio, audio_fps=input_audio_fps)
  print("Keeping speaking intervals: " + str(speaking_intervals))
  speaking_clips = [input_clip.subclip(start, end) for [start, end] in speaking_intervals]
  final_video = concatenate_videoclips(speaking_clips)
  return final_video  

def main():
  sts = speech_to_text()
  word_ts = word_level_timestamps(sts)
  commands = find_commands(word_ts)
  print(f'Commands: {commands}')
  keep_intervals = intervals_to_keep(commands)
  print(f'Keeping intervals: {keep_intervals}')

  vid = VideoFileClip(input_file)

  # Edit with speech-to-text.
  keep_clips = [vid.subclip(start, end) for [start, end] in keep_intervals]
  edited_vid = concatenate_videoclips(keep_clips)

  # Cut out dead air.
  no_dead_air_video = find_speaking(edited_vid, vid.audio.fps)

  print("\n\n\n----- Writing out edited video... -----")
  no_dead_air_video.write_videofile(output_file,
  #edited_vid.write_videofile(output_file,
        #fps=60,
        preset='ultrafast',
        codec='libx264',
        #codec='h264_videotoolbox',
        temp_audiofile='temp-audio.m4a',
        remove_temp=True,
        audio_codec="aac",
        #threads=6,
        ffmpeg_params = ['-threads', '8'],
    )
  vid.close()


if __name__ == '__main__':
  main()
