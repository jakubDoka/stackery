import bisect

def generate_color_commands(text, ranges):
    # Sort the ranges in ascending order by start position
    sorted_ranges = sorted(ranges, key=lambda r: r[0])

    # Initialize the list of color commands
    commands = []

    # Iterate over the sorted ranges and insert/split to the ranges list
    ranges_list = []
    last_inserted_range_index = -1
    for i, (range_start, range_end, color, is_background) in enumerate(sorted_ranges):
        # Skip searching within ranges that come before the last inserted range
        search_start_index = last_inserted_range_index + 1
        ranges_list = insert_range(ranges_list, range_start, range_end, color, is_background, search_start_index)
        last_inserted_range_index = i

    # Generate the color commands for the ranges
    for i in range(len(text)):
        fg_color, bg_color = None, None
        for start, end, color, is_background in ranges_list:
            if start <= i < end:
                if is_background:
                    bg_color = color
                else:
                    fg_color = color
        if bg_color is not None:
            command = f"\033[48;2;{bg_color[0]};{bg_color[1]};{bg_color[2]};2m"
            commands.append(command)
        if fg_color is not None:
            command = f"\033[38;2;{fg_color[0]};{fg_color[1]};{fg_color[2]}m"
            commands.append(command)
        commands.append(text[i])
    commands.append("\033[0m")

    return "".join(commands)

def insert_range(ranges, range_start, range_end, color, is_background, search_start_index=0):
    i = bisect.bisect_left(ranges, (range_start,), search_start_index)
    if i < len(ranges):
        start, end, range_color, _ = ranges[i]
        if start < range_end:
            # Range overlaps with an existing range, split it into two ranges
            ranges[i:i+1] = [(start, range_start, range_color, is_background),
                             (range_start, min(end, range_end), color, is_background),
                             (min(end, range_end), end, range_color, is_background)]
            return ranges
    # Range doesn't overlap with any existing ranges, insert it
    ranges.insert(i, (range_start, range_end, color, is_background))
    return ranges
