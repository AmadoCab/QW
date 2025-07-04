(* ::Package:: *)

Quiet[
MakeUsageString::usage=FormatUsage@"MakeUsageString[boxes] converts the box expression returned by [*ParseFormatting*] to a string that can be used as usage message.
MakeUsageString[{boxes_1,\[Ellipsis]}] creates a multiline string, with line ```i``` corresponding to ```boxes_i```.";,
{FrontEndObject::notavail,First::normal}
];
