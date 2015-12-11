#! /usr/bin/python
from scapy.all import *

def arp_display(pkt):
  if pkt[ARP].op == 1: #who-has (request)
    if pkt[ARP].psrc == '0.0.0.0': # ARP Probe
	  # Remember to change the ff address to the address from your button
      if pkt[ARP].hwsrc == 'ff:ff:ff:ff:ff:ff':
        print "Pushed Huggies"
      else:
        print "ARP Probe from unknown device: " + pkt[ARP].hwsrc
print sniff(prn=arp_display, filter="arp", store=0, count=10)
