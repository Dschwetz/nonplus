<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>Diaper Tracker</title>
</head>

<body style="background-color: #111111; color: BlanchedAlmond; font-family: 'Lucida Console', Monaco, monospace">
<h2>Do I need diapers?</h2>
<?php
date_default_timezone_set("America/New_York");
	
function pcdf ($k, $lambda) {
	// Sums up the probability that we will use <= k
	// Specifically, P(X <= x)
	$st = 0;
	$res = 0.0;
	$cdf_val = 0.0;
	if ($k < 0) {
		$cdf_val = 0;
	} else {
		while ($st <= $k) {
			if ($st == 0) {
				$res = exp(-$lambda);           // f(0)
			} else {
				$res = ($res * $lambda) / $st;  // f(1,2...k)
			}
			$st = $st + 1;
			$cdf_val = $cdf_val + $res;
		}
	}
	return ($cdf_val);
}

function running_out_chance($days, $onhand, $lambda) {
	// Answers the question, What is the probability I will use more than the onhand amount?
	// Which, put another way, asks what is the chance I will need and not have?
	
	$certain_diapers_per_day = 2;
	$random_part = ($onhand - $certain_diapers_per_day*$days);
	$parameter = $lambda*$days;

	if ($random_part >= 0) { 
		return (1 - pcdf( $random_part, $parameter));
	} else {
		return 1; 			//Will deterministically run out.
	}
}

$diapers_on_hand = 0;			//Initialization
$dbconn = pg_connect("host=192.168.1.2 port=5432 dbname=yourdbname user=yourdbuser password=yourdbpass");
if (!$dbconn) {
	die(pg_last_error($dbconn));
}
if ($dbconn) {
	// On hand query
	$query = "SELECT numonhand from diapersonhand where deviceid = 1";
	$result = pg_query($query) or die('Query failed: ' . pg_last_error());
	$line = pg_fetch_array($result, null, PGSQL_ASSOC);
	$diapers_on_hand = $line["numonhand"];	
	
	// Parameter query
	$query = "WITH counttable AS (
				SELECT count(diaperid) - 3 as counts
				from diapertrack
				group by date_trunc('day', diapertick)
				order by date_trunc('day', diapertick)
            )
			SELECT avg(counttable.counts) as lambda
			from counttable";
	$result = pg_query($query) or die('Query failed: ' . pg_last_error());
	$line = pg_fetch_array($result, null, PGSQL_ASSOC);
	$lam = $line["lambda"];	
}

// ---------------------------------------------- //
// Manually hardcoding for example only -- remove 
$lam = 0.5;
$diapers_on_hand = 10;
// ---------------------------------------------- //

echo "<h3>Current diapers on hand: $diapers_on_hand </h3>";
echo "Chance of running out:<br><br>";
echo "+--------------------------+<br>";
for ($i = 1; $i <= 8; $i++) {
	//Just to keep the code clean when referencing day 0 (today), tomorrow, 2 days in the future, etc.
	// Though today is "day 0," it is still 1 day according to the math, so i = 1, but our "day" is day 0.
	$day = $i - 1;	
	
	$day_chance = running_out_chance($i, $diapers_on_hand, $lam)*100;
	$day_chance = abs(number_format($day_chance,2));	//Some noise while rounding really,really small numbers goes negative. 
	$extra_space = "";
	if ($day_chance < 10) {
		$extra_space = "&nbsp;&nbsp;";
	} else if ($day_chance < 100){
		$extra_space = "&nbsp;";	
	}
	$day_of_week = date('D', time()+ (($day*24*60*60)));
	if ($day_of_week == "Sun" || $day_of_week == "Sat") {
		$format_string = "| <span style=\"color:cornflowerblue\">";
	} else {
		$format_string = "| <span style=\"color:BlanchedAlmond;\">";
	}
	if ($i == 1) {
		$day_string = "Today ###";
	} else {
		$day_string = "In $day days";
	}
	echo $format_string . "$day_string [$day_of_week]: $day_chance% $extra_space</span>|<br>";
}
echo "+--------------------------+";
$expect = floor($diapers_on_hand / ($lam + 3));
echo "<h3>Expected number of days left: $expect </h3>";
?>
</body>