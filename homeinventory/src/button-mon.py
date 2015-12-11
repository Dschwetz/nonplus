#! /usr/bin/python
from scapy.all import *
import psycopg2

def arp_display(pkt):
  if pkt[ARP].op == 1: #who-has (request)
    if pkt[ARP].psrc == '0.0.0.0': # ARP Probe
      if pkt[ARP].hwsrc == 'ff:ff:ff:ff:ff:ff':
        newt = time.time()
        print "Pushed Huggies"
        try:
          conn=psycopg2.connect("dbname='yourdbname' user='yourdbuser' password='yourdbpw'")
          print "Connected!"
          cur = conn.cursor()
          cur.execute("INSERT INTO diapertrack (diapertick, diaperdeviceid) VALUES (now(), 1);")
          conn.commit()
          cur.close()
          conn.close()
          print "Complete."
        except:
          print "Unable to connect to the database."
      else:
        print "ARP Probe from unknown device: " + pkt[ARP].hwsrc

# Removed count=10 to allow to loop forever
print sniff(prn=arp_display, filter="arp", store=0)
