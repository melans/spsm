<style>table,tr,td{border:1px solid;text-align:center;white-space:nowrap}</style>
<body topmargin=0 leftmargin=0>
<table><tr><td>L
<script>
M=arr("Months,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec")

for(m=1;m<13;m++)W("<td><button onclick='select("+m+")'>"+M[m]+"")

for(l=1;l<8;l++){
W("<tr><td>"+l)
for(m=1;m<13;m++){
x=m+l-1
x=x>12?x-12:x
B("<td "+(x==10?"bgcolor=yellow":"")+" >Y."+m+"."+l+" = X."+m+"."+l+" + Y."+m+"."+(l-1))
B((x)+" "+((x)%12)+" = "+M[x])
//W("<hr>Q("+M[x]+") = CPT("+M[x]+") + Q("+M[x-1]+")")
}

}
function select(me){
alert(me)
}

	function _(s){return(document.getElementById(s))}
	function __(s){return(document.getElementsByName(s))}
	function ___(s){return(document.getElementsByTagName(s))}
	function ____(s){return(document.getElementsByClassName(s))}
	function W(s){document.write(s)}
	function WW(){s="";for(i=0; i<arguments.length; i++)s += i+"="+arguments[i]+", ";W(s)}
	function B(s){document.write(s+'<BR>')}
	function N(s){document.write(s+'\n')}
	function arr(st,sp){return(eval('st.toString().split(/'+(sp||',')+'/g)'))}
	function $() {
		var elements = new Array();
		for (var i = 0; i < arguments.length; i++) {
			var element = arguments[i];
			if (typeof element == 'string')
				element = document.getElementById(element);
			if (arguments.length == 1)
				return element;
			elements.push(element);
		}
		return elements;
	}
</script>
