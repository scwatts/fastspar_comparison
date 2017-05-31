#!/usr/bin/env python3
import argparse
import pathlib


import numpy


class CountData:
    header = list()
    otu_ids = list()
    counts = list()


def get_arguments():
    # Defining parser and arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('-c', '--counts', required=True, type=pathlib.Path,
            help='Input counts file')
    parser.add_argument('-o', '--output_directory', required=True, type=pathlib.Path,
            help='Output directory')
    parser.add_argument('-n', '--number', default=20, type=int,
            help='Number of fraction estimates to perform')
    parser.add_argument('-s', '--seed', default=0, type=int,
            help='Integer used to seed the PRNG')

    # Make sure the input file exists and the output file does NOT exist
    args = parser.parse_args()

    if not args.counts.exists():
        parser.error('Input file %s does not exist' % args.counts)
    if args.output_directory.exists():
        s = 'Output directory %s already exists, use a different one or delete before running'
        parser.error(s % args.output_directory)

    return args


def read_counts_file(counts_fp):
    # Return variable
    count_data = CountData

    # Skip commments until we encounter the header
    with counts_fp.open('r') as f:
        # Find position of the header, skipping any comments
        while True:
            position = f.tell()
            line = f.readline()
            if line.startswith('#'):
                last_position = position
            else:
                break

        # Seek to header and get column names
        f.seek(last_position, 0)
        count_data.header = f.readline().rstrip()

        # Read in data
        for line in f:
            # Separate row names and counts
            otu_id, row_counts = line.rstrip().split('\t', maxsplit=1)

            # Append row name and counts to appropriate lists
            count_data.otu_ids.append(otu_id)
            count_data.counts.append(row_counts.split('\t'))

    return count_data


def generate_fraction_estimates(counts):
    # Return variable
    fraction_estimates = list()

    # Generate fraction estimates
    for count in counts:
        # Add pseudocount
        alpha = [float(el) + 1 for el in count]

        # Draw from dirichlet distribution parameterised by sample counts with a pseudocount
        theta = numpy.random.mtrand.dirichlet(alpha)
        fraction_estimates.append(theta)

    return fraction_estimates


def write_out_fraction_estimates(fraction_estimates, count_data, output_fp):
    # We must transposes the fraction estimates prior to writing out to retain
    # original storage order
    with output_fp.open('w') as f:
        # Write out the header
        print(count_data.header, file=f)

        # Write out fraction estimates
        for i, fe in enumerate(zip(*fraction_estimates)):
            print(count_data.otu_ids[i], *fe, sep='\t', file=f)


def main():
    # Get input arguments
    args = get_arguments()

    # Read in counts file
    count_data = read_counts_file(args.counts)

    # Create output directory
    args.output_directory.mkdir(0o700)

    # Set PRNG seed
    numpy.random.seed(args.seed)

    # Transpose the count data such that each list contains the counts for each
    # sample rather than OTU
    counts_column_major = list(zip(*count_data.counts))

    # Generate n number of fraction estimates
    for i in range(args.number):
        # Get output filepath for current fraction estimate
        output_fp = pathlib.Path(args.output_directory, 'fraction_estimate_%s.tsv' % i)

        # Draw fraction estimates and write out
        fraction_estimates = generate_fraction_estimates(counts_column_major)
        write_out_fraction_estimates(fraction_estimates, count_data, output_fp)


if __name__ == '__main__':
    main()
