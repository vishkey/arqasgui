function PrintElem(elem)
    {
        Popup($(elem).html());
    }

 function Popup(data) 
    {
        var mywindow = window.open('', '_blank');
        mywindow.document.write('<html>');
        /*optional stylesheet*/ //mywindow.document.write('<link rel="stylesheet" href="main.css" type="text/css" />');
        mywindow.document.write('<body >');
        mywindow.document.write(data);
        mywindow.document.write('</body></html>');

        mywindow.print();
        mywindow.close();

        return true;
    }